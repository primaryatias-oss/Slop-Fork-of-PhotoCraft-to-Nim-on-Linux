#!/usr/bin/env python3
"""Generates random engine scripts for tools/enginecheck.nim (nim/PORTING.md).

`enginecases.py OUTDIR COUNT [SEED] [AREAS]`: writes COUNT scripts (`case-N.json`), each a list of
`{"cmd", "params"}` steps: a document or two, then random commands from the areas ported so far,
valid and hostile params mixed. AREAS (comma-separated) limits the command pool.
"""
import json, os, random, sys

def color(r):
    return r.choice(["#ff0000", "#00ff80", "#123456", "#ffffff80", [0.2, 0.4, 0.6], [1, 0, 0, 0.5], "bad", 7, None])

def rect(r, big=False):
    m = 300 if big else 70
    return {"x": r.randint(-20, m), "y": r.randint(-20, m), "width": r.randint(0, m), "height": r.randint(0, m)}

def layer_id(r):
    return r.choice([r.randint(1, 30), r.randint(1, 30), "x", -1, 2**40, None])

def new_doc(r):
    p = {"width": r.choice([16, 40, 64, 97, 300, 1.5, 0, -3, "x"]), "height": r.choice([16, 33, 48, 128, 2.6])}
    if r.random() < 0.5: p["mode"] = r.choice(["rgb", "gray", "grayscale", "cmyk", "lab", "bitmap", 3])
    if r.random() < 0.5: p["depth"] = r.choice([8, 16, 32, 7, "8"])
    if r.random() < 0.5: p["background"] = r.choice(["white", "black", "transparent", "backgroundColor", "#336699", "#zz0000", "#12345", 1])
    if r.random() < 0.2: p["backgroundColor"] = r.choice([[0.1, 0.2, 0.3], [1, 2, 3], [0.5], "x"])
    if r.random() < 0.2: p["resolution"] = r.choice([72, 300, 0, 1e9])
    if r.random() < 0.2: p["name"] = r.choice(["Doc", "a.b.psd", 5])
    if r.random() < 0.1: p["bogus"] = 1
    return {"cmd": "file.new", "params": p}

def valid_doc(r):
    p = {"width": r.choice([16, 40, 64, 97]), "height": r.choice([16, 33, 48])}
    if r.random() < 0.5: p["mode"] = r.choice(["rgb", "rgb", "gray", "cmyk", "lab"])
    if r.random() < 0.5: p["depth"] = r.choice([8, 16, 32])
    if r.random() < 0.5: p["background"] = r.choice(["white", "black", "transparent", "#336699"])
    return {"cmd": "file.new", "params": p}

def step(r, areas):
    pool = []
    def add(area, w, f):
        if area in areas: pool.append((w, f))
    add("select", 6, lambda: {"cmd": "select.rect", "params": dict(rect(r), **({"mode": r.choice(["add", "subtract", "intersect", "replace", "x"])} if r.random() < 0.4 else {}), **({"ellipse": True} if r.random() < 0.3 else {}), **({"feather": r.choice([0, 2.5, 9])} if r.random() < 0.2 else {}), **({"line": r.choice(["row", "column", "z"])} if r.random() < 0.1 else {}), **({"antiAlias": False} if r.random() < 0.1 else {}))})
    add("select", 2, lambda: {"cmd": r.choice(["select.all", "select.deselect", "select.inverse"])})
    add("layer", 5, lambda: {"cmd": "layer.new.layer", "params": r.choice([{}, {"name": "L"}, {"mode": r.choice(["Multiply", "Overlay", "Screen", "Hue", "nope"]), "fillNeutral": r.random() < 0.5}, {"opacity": r.choice([0.5, 2, "x"])}, {"color": r.choice(["red", "blue", "plaid", 3])}, {"clipToPrevious": r.choice([True, 1])}])})
    add("layer", 2, lambda: {"cmd": "layer.new.group", "params": r.choice([{}, {"name": "G"}])})
    add("layer", 3, lambda: {"cmd": "layer.select", "params": {"layer": layer_id(r), "mode": r.choice(["replace", "add", "toggle", "range", "zz"])}})
    add("layer", 3, lambda: {"cmd": "layer.setProps", "params": r.choice([{"opacity": 0.5}, {"blend": r.choice(["Multiply", "Linear Dodge (Add)", "color-burn", "bad"])}, {"visible": False}, {"name": "N"}, {"fill": 0.3}, {"clipped": True}, {"locks": {"pixels": True}}, {"locks": {"zz": True}}, {"channels": [True, False, True]}, {"layer": layer_id(r), "visible": True}])})
    add("layer", 2, lambda: {"cmd": r.choice(["layer.duplicate", "layer.delete", "layer.mergeDown", "layer.groupLayers", "layer.createClippingMask", "layer.releaseClippingMask"]), "params": r.choice([{}, {"layer": layer_id(r)}])})
    add("layer", 2, lambda: {"cmd": r.choice(["layer.arrange.bringForward", "layer.arrange.sendBackward", "layer.arrange.bringToFront", "layer.arrange.sendToBack", "layer.arrange.reverse"])})
    add("layer", 2, lambda: {"cmd": r.choice(["layer.layerMask.revealAll", "layer.layerMask.hideAll", "layer.layerMask.revealSelection", "layer.layerMask.delete"])})
    add("layer", 2, lambda: {"cmd": "layer.translate", "params": {"dx": r.randint(-40, 40), "dy": r.choice([r.randint(-40, 40), 2**33, 1.6])}})
    add("layer", 1, lambda: {"cmd": "layer.moveTo", "params": {"target": layer_id(r), "position": r.choice(["above", "below", "into", "x"]), **({"copy": True} if r.random() < 0.3 else {})}})
    add("layer", 1, lambda: {"cmd": r.choice(["layer.flattenImage", "select.allLayers", "select.deselectLayers", "layer.mergeLayers", "layer.linkLayers", "layer.selectLinkedLayers", "layer.new.backgroundFromLayer"])})
    add("layer", 1, lambda: {"cmd": "layer.lockLayers", "params": r.choice([{}, {"pixels": True}, {"all": False}])})
    add("layer", 1, lambda: {"cmd": "layer.renameLayer", "params": {"name": r.choice(["R", "", 3])}})
    add("layer", 1, lambda: {"cmd": "select.findLayers", "params": {"name": r.choice(["layer", "G", "zzz"])}})
    add("layer", 1, lambda: {"cmd": "layer.align." + r.choice(["topEdges", "verticalCenters", "bottomEdges", "leftEdges", "horizontalCenters", "rightEdges"]), "params": r.choice([{}, {"to": r.choice(["canvas", "selection", "layers", "q"])}])})
    add("layer", 1, lambda: {"cmd": "layer.distribute." + r.choice(["topEdges", "rightEdges", "horizontally", "vertically"])})
    add("fill", 6, lambda: {"cmd": "edit.fill", "params": r.choice([{}, {"contents": r.choice(["foreground", "background", "black", "gray", "white", "Pattern", "history", "zz", 3])}, {"color": color(r)}, {"contents": "color", "color": color(r), "mode": r.choice(["multiply", "dissolve", "screen", "nope"]), "opacity": r.choice([50, 100, 0, 250, "x"])}, {"contents": "white", "preserveTransparency": r.choice([True, "yes"])}, {"contents": "pattern", "pattern": r.choice(["Bricks", "Dots", "nope", ""]), "scale": r.choice([50, 200]), "angle": r.choice([0, 30])}, {"contents": "history", "state": r.choice([0, 1, 99, "x"])}, {"contents": "contentAware"}])})
    add("fill", 2, lambda: {"cmd": "edit.clear"})
    add("fill", 1, lambda: {"cmd": "tools.setColors", "params": {"foreground": color(r), "background": color(r)}})
    add("fill", 1, lambda: {"cmd": r.choice(["tools.swapColors", "tools.defaultColors"])})
    add("history", 3, lambda: {"cmd": r.choice(["edit.undo", "edit.undo", "edit.redo"])})
    add("image", 1, lambda: {"cmd": "image.imageRotation." + r.choice(["flipCanvasHorizontal", "flipCanvasVertical", "180", "90cw", "90ccw"])})
    add("channel", 2, lambda: {"cmd": "channel.new", "params": r.choice([{}, {"fill": r.choice(["white", "selection", "black"])}, {"name": "A", "color": "#00ff00", "opacity": 70, "indicates": "selected"}])})
    add("channel", 1, lambda: {"cmd": "channel.newSpot", "params": r.choice([{}, {"color": "#ff00ff", "solidity": 40}])})
    add("channel", 2, lambda: {"cmd": "channel.target", "params": {"channel": r.choice(["composite", "red", "green", "Blue", "gray", "cyan", 0, 1, 7, "mask", "Alpha 1", "nope"])}})
    add("channel", 1, lambda: {"cmd": "channel.setVisible", "params": {"channel": r.choice(["composite", "red", 0, "quickMask", "zz"]), **({"visible": r.choice([True, False])} if r.random() < 0.5 else {})}})
    add("channel", 1, lambda: {"cmd": r.choice(["channel.target.composite", "channel.target.slot3", "channel.target.slot4", "channel.target.slot6", "channel.list"])})
    add("channel", 1, lambda: {"cmd": "select.saveSelection", "params": r.choice([{}, {"name": "Saved"}, {"channel": 0, "operation": r.choice(["add", "subtract", "intersect", "replace", "q"])}])})
    add("channel", 1, lambda: {"cmd": "select.loadSelection", "params": {"channel": r.choice([0, "Alpha 1", "transparency", "red", "composite", "mask", "quickMask", "selection", 9]), "operation": r.choice(["new", "add", "subtract", "intersect", "x"]), **({"invert": True} if r.random() < 0.3 else {})}})
    add("channel", 1, lambda: {"cmd": "select.editInQuickMaskMode", "params": r.choice([{}, {"on": True}, {"on": False}])})
    add("channel", 1, lambda: {"cmd": r.choice(["channel.delete", "channel.rename", "channel.move", "channel.duplicate", "channel.options", "channel.mergeSpot"]), "params": r.choice([{}, {"channel": 0, "name": "Ren", "to": 1}, {"channel": "red"}, {"channel": "quickMask", "color": "#0000ff", "opacity": 30, "indicates": r.choice(["selected", "masked", "spot"])}, {"channel": 0, "indicates": "spot", "solidity": 50}])})
    add("channel", 1, lambda: {"cmd": "image.applyImage", "params": r.choice([{}, {"blending": r.choice(["screen", "add", "subtract", "difference", "dissolve", "hue"]), "opacity": 60, "source": {"channel": r.choice(["red", "composite", 0, "transparency"])}}, {"sourceChannel": "green", "preserveTransparency": True, "scale": 1.5, "offset": 20}])})
    add("channel", 1, lambda: {"cmd": "image.calculations", "params": r.choice([{}, {"source1": {"channel": "red"}, "source2": {"channel": "blue", "invert": True}, "blending": "multiply", "result": r.choice(["newChannel", "selection", "newDocument", "zz"])}])})
    add("channel", 1, lambda: {"cmd": r.choice(["channel.split", "channel.merge"]), "params": r.choice([{}, {"mode": "rgb"}, {"closeOriginal": False}])})
    add("pattern", 2, lambda: {"cmd": "layer.newFillLayer.pattern", "params": r.choice([{}, {"pattern": r.choice(["Bricks", "Weave", "Paper Noise", "nope"]), "scale": 150, "angle": 20, "phase": [3, 4]}])})
    add("pattern", 1, lambda: {"cmd": r.choice(["pattern.list", "edit.definePattern", "pattern.rename", "pattern.delete"]), "params": r.choice([{}, {"pattern": "Grid", "name": "Grid 2"}, {"rect": [0, 0, 8, 8], "name": "Mine"}])})
    add("pattern", 1, lambda: {"cmd": "brush.texturePattern", "params": {"pattern": r.choice(["Dots", "Checkerboard", "none"]), "scale": 50}})
    add("misc", 1, lambda: {"cmd": r.choice(["document.inspect", "session.inspect", "document.pixel", "view.layerMask", "layer.toggleMaskOverlay"]), "params": r.choice([{}, {"x": 3, "y": 4}, {"mode": r.choice(["gray", "overlay", "off", "toggleGray", "q"])}])})
    add("misc", 1, lambda: {"cmd": r.choice(["document.activate", "document.move", "file.close"]), "params": r.choice([{}, {"document": 0}, {"document": 1, "to": 0}, {"document": 9}])})
    add("misc", 1, lambda: {"cmd": "image.adjustments.desaturate"})
    add("edit", 3, lambda: {"cmd": r.choice(["edit.copy", "edit.cut", "edit.copyMerged", "edit.paste", "edit.pasteSpecial.pasteInPlace", "file.newFromClipboard"]), "params": r.choice([{}, {}, {"center": [10, 12]}, {"target": r.choice(["mask", "quickMask", {"channel": 0}])}])})
    add("edit", 2, lambda: {"cmd": r.choice(["layer.new.layerViaCopy", "layer.new.layerViaCut", "layer.mergeVisible", "image.autoTone", "image.autoContrast", "image.autoColor", "edit.toggleLastState"])})
    add("edit", 1, lambda: {"cmd": r.choice(["view.newGuide", "view.moveGuide", "view.deleteGuide", "view.clearGuides"]), "params": {"orientation": r.choice(["horizontal", "vertical"]), "position": r.choice([10, 33.5, "50%", "1 in", "2.5 cm", "12 pt", "x", "1e9"]), "index": r.choice([0, 1, 5]), **({"color": r.choice(["cyan", "Magenta", "#ff8800", "nope"])} if r.random() < 0.4 else {})}})
    add("extra", 2, lambda: {"cmd": "edit.stroke", "params": r.choice([{}, {"width": r.choice([1, 3, 6.5, 0, 300]), "location": r.choice(["inside", "outside", "center"]), "color": color(r)}, {"mode": r.choice(["multiply", "bad", 2]), "opacity": r.choice([40, "x"])}, {"preserveTransparency": r.choice([True, 1])}])})
    add("extra", 2, lambda: {"cmd": r.choice(["edit.pasteSpecial.pasteInto", "edit.pasteSpecial.pasteOutside", "select.reselect", "image.adjustments.equalize", "layer.new.layerFromBackground", "layer.layerStyle.copyLayerStyle", "layer.layerStyle.pasteLayerStyle", "layer.layerStyle.hideAllEffects", "layer.layerStyle.showAllEffects"])})
    add("extra", 2, lambda: {"cmd": r.choice(["layer.layerMask.enabled", "layer.layerMask.linked", "layer.rasterize.layer", "layer.rasterize.allLayers", "layer.rasterize.fillContent", "layer.delete.hiddenLayers", "file.scripts.deleteAllEmptyLayers", "layer.ungroupLayers", "layer.hideLayers", "layer.showLayers", "layer.showOnly"]), "params": r.choice([{}, {"layer": layer_id(r)}, {"enabled": False}])})
    add("extra", 2, lambda: {"cmd": r.choice(["filter.blur.average", "filter.render.clouds", "filter.render.differenceClouds"]), "params": r.choice([{}, {"seed": r.randint(0, 99)}])})
    add("extra", 1, lambda: {"cmd": r.choice(["layer.newFillLayer.solidColor", "layer.newFillLayer.gradient"]), "params": r.choice([{}, {"color": "#336699"}, {"from": "#ff0000", "to": "#0000ff", "style": r.choice(["linear", "radial"])}])})
    add("color", 1, lambda: {"cmd": r.choice(["edit.assignProfile", "edit.convertToProfile"]), "params": {"profile": r.choice(["srgb", "display-p3", "adobe-rgb-compat", "sgray", "coated-cmyk", "lab-d50", "none", "working", "nope"]), **({"intent": r.choice(["perceptual", "absolute", "zz"])} if r.random() < 0.3 else {})}})
    add("color", 1, lambda: {"cmd": r.choice(["edit.colorSettings", "edit.profileInfo", "view.proofSetup", "view.proofColors", "view.gamutWarning", "color.profileMismatch"]), "params": r.choice([{}, {"workingRgb": "display-p3", "intent": "perceptual"}, {"profile": "coated-cmyk"}, {"on": True, "threshold": 2}, {"action": r.choice(["convert", "discard", "assignWorking", "zz"])}, {"blendTextGamma": r.choice([1.2, False, 9])}, {"reset": True}])})
    def fx_params():
        q = {}
        for k, vals in [("color", ["#ff0000", "#00ff00", "zz", 3]), ("opacity", [0, 40, 100, 150, "x"]), ("blend", ["multiply", "screen", "normal", "nope"]),
                        ("angle", [0, 45, -90, 400]), ("distance", [0, 5, 12, -3]), ("size", [0, 3, 9, 300]), ("spread", [0, 50, 120]), ("choke", [10, 0]),
                        ("contour", ["Linear", "Cone", "Gaussian", "Ring - Double", "nope", {"name": "C", "points": [[0, 0], [0.5, 1], [1, 0]]}, {"points": [[0, 2]]}]),
                        ("antiAlias", [True, False]), ("noise", [0, 30]), ("technique", ["softer", "precise", "zz"]), ("source", ["edge", "center"]),
                        ("position", ["outside", "inside", "center", "q"]), ("style", ["linear", "radial", "angle", "inner", "outer", "emboss", "pillow", "x"]),
                        ("from", ["#000000", "#ff8800"]), ("to", ["#ffffff", "#0000ff"]), ("scale", [10, 100, 150, 2000]), ("reverse", [True]),
                        ("pattern", ["Bricks", "Dots", "nope"]), ("depth", [1, 100, 5000]), ("direction", ["up", "down"]), ("soften", [0, 4]),
                        ("altitude", [30, 90]), ("invert", [True]), ("add", [True, False]), ("useGlobalLight", [True, False]), ("knocksOut", [True])]:
            if r.random() < 0.15: q[k] = r.choice(vals)
        return q
    kinds = ["dropShadow", "innerShadow", "outerGlow", "innerGlow", "stroke", "colorOverlay", "gradientOverlay", "patternOverlay", "bevelEmboss", "satin"]
    add("style", 5, lambda: {"cmd": "layer.layerStyle." + r.choice(kinds), "params": dict(fx_params(), **({"layer": layer_id(r)} if r.random() < 0.2 else {}))})
    add("style", 1, lambda: {"cmd": "layer.layerStyle.replace", "params": {"effects": [{"kind": r.choice(kinds + ["nope"]), "params": fx_params()} for _ in range(r.randint(0, 3))], **({"layer": layer_id(r)} if r.random() < 0.2 else {})}})
    add("style", 1, lambda: {"cmd": "layer.layerStyle.makeDefault", "params": {"kind": r.choice(kinds + ["zz"]), "params": fx_params()}})
    add("style", 1, lambda: {"cmd": "layer.layerStyle.defaultFor", "params": {"kind": r.choice(kinds + ["zz", 3])}})
    add("style", 1, lambda: {"cmd": "layer.layerStyle.clear", "params": r.choice([{}, {"effect": r.choice([0, 1, 5, "x"])}, {"layer": layer_id(r)}])})
    add("style", 1, lambda: {"cmd": r.choice(["layer.layerStyle.copyLayerStyle", "layer.layerStyle.pasteLayerStyle", "layer.layerStyle.hideAllEffects", "layer.layerStyle.showAllEffects", "layer.new.layer", "edit.fill"])})
    def adj_params(kind):
        q = {}
        common = {
            "brightnessContrast": [("brightness", [-200, -50, 0, 30, 150, "x"]), ("contrast", [-80, 0, 50, 120]), ("legacy", [True, False, 1])],
            "levels": [("inBlack", [0, 20, 254, -5]), ("inWhite", [255, 200, 1, 21]), ("gamma", [0.5, 1, 3, 20, "g"]), ("outBlack", [0, 30]), ("outWhite", [255, 220]),
                       ("red", [{"inBlack": 10}, {"gamma": 2}, 5]), ("gray", [{"outWhite": 200}]), ("cyan", [{"inBlack": 3}]), ("lightness", [{"gamma": 1.5}]), ("a", [{}])],
            "curves": [("points", [[[0, 0], [128, 160], [255, 255]], [[0, 10], [255, 240]], [[1, 1]], [[0, 0], [0.2, 5], [255, 255]], "x", [i * 1.0 for i in range(256)]]),
                       ("red", [[[0, 0], [64, 90], [255, 255]]]), ("pencil", [True, False]), ("smooth", [1, 3, 20]), ("black", [[[0, 0], [255, 200]]]), ("b", [[[0, 0], [255, 250]]])],
            "exposure": [("exposure", [-30, 0, 1.5]), ("offset", [-1, 0.1]), ("gamma", [0, 1, 2])],
            "vibrance": [("vibrance", [-150, 30]), ("saturation", [50, -20])],
            "hueSaturation": [("hue", [-200, 40, 300]), ("saturation", [-100, 30, 200]), ("lightness", [-20, 10]), ("colorize", [True, False]),
                              ("reds", [{"hue": 20, "range": [300, 340, 20, 50]}, {"saturation": -30}, 4]), ("greens", [{"range": [100, 90, 200, 150]}])],
            "colorBalance": [("shadows", [[10, -20, 30], [1, 2]]), ("midtones", [[200, 0, -200]]), ("highlights", [[0, 0, 5]]), ("preserveLuminosity", [True, False])],
            "blackWhite": [("reds", [10, 400, -300]), ("blues", [50]), ("tint", [True, False]), ("tintColor", ["#ff8800", [0.2, 0.3, 0.4], "zz", "##aabbcc", "#+f+f+f"])],
            "photoFilter": [("filter", ["warming85", "sepia", "underwater", "nope", 3]), ("color", ["#00ff00", [1, 0, 0]]), ("density", [0, 50, 150]), ("preserveLuminosity", [False])],
            "channelMixer": [("monochrome", [True, False]), ("red", [[100, 0, 0, 0], [50, 50, 0, 10]]), ("gray", [[40, 40, 20, 0], [300, 0, 0, 0]]), ("blue", [[1, 2, 3]])],
            "gradientMap": [("stops", [[[0, "#000000"], [1, "#ff0000"]], [{"location": 0.5, "color": [0, 1, 0]}, [0, "#fff"]], [[0.3, "#123456"]], "x"]), ("reverse", [True]), ("dither", [True])],
            "selectiveColor": [("method", ["relative", "absolute", "q"]), ("reds", [[10, 20, -30, 5], [1, 2]]), ("colors", ["blues", "neutrals", "zz"]), ("cyan", [40]), ("black", [-120])],
            "colorLookup": [("lut", ["warm", "cool", "none", "tealOrange", "zz"]), ("interpolation", ["trilinear", "tetrahedral", "x"]), ("dither", [True]),
                            ("data", ["LUT_3D_SIZE 2\n0 0 0\n1 0 0\n0 1 0\n1 1 0\n0 0 1\n1 0 1\n0 1 1\n1 1 1\n", "garbage"])],
            "threshold": [("level", [0, 128, 300])], "posterize": [("levels", [1, 4, 300])], "invert": [],
        }.get(kind, [])
        for k, vals in common:
            if r.random() < 0.35: q[k] = r.choice(vals)
        if kind in ("levels", "curves") and r.random() < 0.25:
            q["eyedropper"] = r.choice([{"point": r.choice(["black", "gray", "white", "zz"]), "color": [r.random(), r.random(), r.random()]},
                                        {"point": r.choice(["black", "gray", "white"]), "at": [r.randint(-2, 50), r.randint(0, 40)], **({"size": r.choice([1, 3, 5, 7])} if r.random() < 0.5 else {})},
                                        {"point": "white"}])
        return q
    akinds = ["brightnessContrast", "levels", "curves", "exposure", "vibrance", "hueSaturation", "colorBalance", "blackWhite", "photoFilter", "channelMixer",
              "gradientMap", "selectiveColor", "colorLookup", "threshold", "posterize", "invert"]
    def adj_step(prefix):
        k = r.choice(akinds)
        return {"cmd": prefix + k, "params": adj_params(k)}
    add("adjust", 4, lambda: adj_step("layer.newAdjustmentLayer."))
    add("adjust", 4, lambda: adj_step("image.adjustments."))
    add("adjust", 3, lambda: {"cmd": "layer.setAdjustment", "params": dict(adj_params(r.choice(akinds)), **({"layer": layer_id(r)} if r.random() < 0.3 else {}))})
    add("adjust", 2, lambda: {"cmd": r.choice(["image.adjustments.shadowsHighlights", "image.adjustments.replaceColor", "image.adjustments.matchColor", "image.adjustments.hdrToning"]),
                              "params": r.choice([{}, {"shadowAmount": 60, "highlightAmount": 20, "color": 50}, {"color": "#ff0000", "fuzziness": 80, "hue": 30}, {"source": r.choice([0, 1, 5]), "fade": 20, "neutralize": True},
                                                  {"radius": 5, "strength": 1, "curve": [[0, 0], [128, 150], [255, 255]]}, {"useSelectionInTarget": True, "luminance": 150}])})
    add("adjust", 1, lambda: {"cmd": r.choice(["image.adjustments.colorLookup.list", "document.sampleColor"]), "params": {"x": r.choice([0, 3.5, 20, -1, 1e9, "x"]), "y": r.choice([0, 7, 39.9]), **({"size": r.choice([1, 3, 5, 11, "point", 2, 101])} if r.random() < 0.6 else {}), **({"sampleLayer": r.choice(["current", "currentAndBelow", "all", "allNoAdjustments", "currentAndBelowNoAdjustments", "zz", 3])} if r.random() < 0.5 else {})}})
    fids = {
        "filter.blur.gaussianBlur": [("radius", [0.5, 2, 5, 0, 2000, "x"])], "filter.blur.blur": [], "filter.blur.blurMore": [],
        "filter.sharpen.sharpen": [], "filter.sharpen.sharpenMore": [], "filter.sharpen.sharpenEdges": [], "filter.noise.despeckle": [],
        "filter.blur.boxBlur": [("radius", [1, 3, 0.5])], "filter.blur.motionBlur": [("angle", [0, 30, 400]), ("distance", [3, 12])],
        "filter.blur.radialBlur": [("amount", [5, 30]), ("method", ["spin", "zoom", "q"]), ("quality", ["draft", "good", "best"]), ("centerX", [0.3])],
        "filter.blur.surfaceBlur": [("radius", [2, 5]), ("threshold", [10, 300])],
        "filter.sharpen.unsharpMask": [("amount", [80, 600]), ("radius", [1.5]), ("threshold", [3])],
        "filter.sharpen.smartSharpen": [("amount", [150]), ("radius", [2]), ("reduceNoise", [20])],
        "filter.noise.addNoise": [("amount", [10, 50]), ("distribution", ["uniform", "gaussian", 3]), ("monochromatic", [True, 1]), ("seed", [0, 7, 4294967295, -1, 2**33])],
        "filter.noise.median": [("radius", [1, 3])], "filter.noise.dustAndScratches": [("radius", [2]), ("threshold", [10])],
        "filter.pixelate.mosaic": [("cellSize", [4, 300])], "filter.stylize.emboss": [("angle", [45]), ("height", [2]), ("amount", [150])],
        "filter.stylize.findEdges": [], "filter.stylize.solarize": [], "filter.distort.twirl": [("angle", [90, -400])], "filter.distort.pinch": [("amount", [50, -50])],
        "filter.distort.spherize": [("amount", [60]), ("mode", ["normal", "horizontalOnly", "zz"])],
        "filter.distort.wave": [("generators", [2, 5]), ("wavelengthMin", [5]), ("wavelengthMax", [40]), ("type", ["sine", "square", "triangle"]), ("undefinedAreas", ["wrap", "repeat", "transparent"]), ("seed", [3])],
        "filter.distort.ripple": [("amount", [100, -300]), ("size", ["small", "large"])], "filter.distort.polarCoordinates": [("mode", ["rectangularToPolar", "polarToRectangular"])],
        "filter.other.highPass": [("radius", [3])], "filter.other.minimum": [("radius", [1, 3]), ("preserve", ["squareness", "roundness"])], "filter.other.maximum": [("radius", [2])],
        "filter.other.offset": [("horizontal", [5, -7, 3e9, 2.5]), ("vertical", [3]), ("undefinedAreas", ["wrap", "repeat", "transparent"])],
        "filter.pixelate.colorHalftone": [("maxRadius", [5])], "filter.pixelate.crystallize": [("cellSize", [6]), ("seed", [2, -3])], "filter.pixelate.facet": [], "filter.pixelate.fragment": [],
        "filter.pixelate.mezzotint": [("type", ["fineDots", "longStrokes", "zz"]), ("seed", [1])], "filter.pixelate.pointillize": [("cellSize", [4]), ("background", ["#ff0000", [0, 1, 0]])],
        "filter.stylize.diffuse": [("mode", ["normal", "darkenOnly", "anisotropic"]), ("seed", [5])],
        "filter.stylize.extrude": [("type", ["blocks", "pyramids"]), ("size", [8]), ("depth", [10]), ("depthMode", ["random", "levelBased"]), ("solidFrontFaces", [True])],
        "filter.stylize.oilPaint": [("stylization", [2]), ("lighting", [False])], "filter.stylize.tiles": [("count", [4]), ("fill", ["background", "inverse", "foreground"]), ("seed", [9])],
        "filter.stylize.traceContour": [("level", [100]), ("edge", ["upper", "lower"])], "filter.stylize.wind": [("method", ["wind", "blast", "stagger"]), ("direction", ["fromLeft"])],
        "filter.distort.displace": [("mapDocument", [0, 1, "Untitled-1", 9]), ("horizontal", [5]), ("fit", ["tile", "stretch"])],
        "filter.distort.shear": [("amount", [30]), ("points", [[[0, 0], [0.5, 0.2], [1, 0]], [[1, 2, 3]]])], "filter.distort.zigZag": [("amount", [20]), ("style", ["aroundCenter", "outFromCenter"])],
        "filter.render.fibers": [("variance", [8]), ("seed", [4])], "filter.render.lensFlare": [("brightness", [120]), ("lens", ["prime35", "moviePrime"])],
        "filter.render.lightingEffects": [("lightType", ["spot", "point", "infinite"]), ("texture", ["none", "luminance"]), ("lights", [[{"kind": "point", "x": 0.3, "y": 0.4}], [5]])],
        "filter.render.relight": [("angle", [30, 400, "x"]), ("warmth", [20])], "filter.noise.reduceNoise": [("strength", [5])],
        "filter.blur.smartBlur": [("radius", [3]), ("quality", ["low", "high"]), ("mode", ["edgeOnly", "normal"])],
        "filter.blur.lensBlur": [("radius", [4]), ("shape", ["triangle", "octagon"]), ("depthMap", ["none", "transparency", "layerMask"])], "filter.blur.shapeBlur": [("radius", [6]), ("shape", ["star", "ring"])],
        "filter.blurGallery.tiltShift": [("blur", [5])], "filter.blurGallery.irisBlur": [("blur", [5]), ("pins", [[{"x": 0.4, "y": 0.5, "blur": 4}]])],
        "filter.blurGallery.fieldBlur": [("blur", [3])], "filter.blurGallery.spinBlur": [("blurAngle", [20]), ("pins", [[{"x": 0.5, "angle": 500}]])],
        "filter.blurGallery.pathBlur": [("speed", [30]), ("paths", [[{"points": [[0.1, 0.1], [0.9, 0.9]], "speed": 900}]])],
        "filter.other.custom": [("kernel", [[0] * 12 + [2] + [0] * 12, [1, 2, 3]]), ("scale", [2]), ("offset", [10])], "filter.other.hsbHsl": [("inputMode", ["rgb", "hsl"]), ("rowOrder", ["hsb", "rgb"])],
        "filter.video.deInterlace": [("eliminate", ["evenFields"]), ("createBy", ["duplication"])], "filter.video.ntscColors": [],
        "filter.other.colorToAlpha": [("color", ["#ffffff", "#000000", [1, 0, 0], "#zz", [2, 0, 0]]), ("transparencyThreshold", [0, 0.3, 2]), ("opacityThreshold", [1, 0.8])],
        "filter.gallery.cutout": [("levels", [4]), ("edgeSimplicity", [3])], "filter.gallery.glowingEdges": [("edgeWidth", [2])], "filter.gallery.texturizer": [("texture", ["brick", "Canvas", "nope"])],
        "filter.filterGallery": [("effects", [[{"filter": "cutout", "params": {"levels": 4}}, {"filter": "grain", "grainType": "speckle"}], [{"filter": "zz"}], [{"filter": "charcoal", "visible": False}]]), ("list", [True, False])],
    }
    def filt():
        fid = r.choice(list(fids))
        q = {}
        for k, vals in fids[fid]:
            if r.random() < 0.5: q[k] = r.choice(vals)
        if r.random() < 0.05: q["bogus"] = 1
        if r.random() < 0.05: q["layer"] = layer_id(r)
        if r.random() < 0.1: q["target"] = r.choice(["mask", "quickMask", {"channel": 0}, "pixels"])
        return {"cmd": fid, "params": q}
    add("filter", 6, filt)
    add("filter", 1, lambda: {"cmd": "filter.lastFilter"})
    if not pool:
        return {"cmd": "session.inspect"}
    tot = sum(w for w, _ in pool)
    x = r.uniform(0, tot)
    for w, f in pool:
        x -= w
        if x <= 0:
            return f()
    return pool[-1][1]()

def main():
    out, count = sys.argv[1], int(sys.argv[2])
    seed = int(sys.argv[3]) if len(sys.argv) > 3 else 1
    areas = set(sys.argv[4].split(",")) if len(sys.argv) > 4 else {"select", "layer", "fill", "history", "image", "channel", "pattern", "misc", "edit", "extra", "color", "style", "adjust", "filter"}
    os.makedirs(out, exist_ok=True)
    r = random.Random(seed)
    for i in range(count):
        steps = [new_doc(r) if r.random() < 0.3 else valid_doc(r)]
        for _ in range(r.randint(3, 25)):
            steps.append(new_doc(r) if r.random() < 0.03 else step(r, areas))
        # Inspect at the end so view state is compared too.
        steps.append({"cmd": "document.inspect"})
        steps.append({"cmd": "session.inspect"})
        with open(os.path.join(out, f"case-{i}.json"), "w") as f:
            json.dump(steps, f)

main()
