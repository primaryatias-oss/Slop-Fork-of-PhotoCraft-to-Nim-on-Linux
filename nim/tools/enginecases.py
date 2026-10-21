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
    add("image", 2, lambda: {"cmd": "image.imageSize", "params": r.choice([{}, {"width": r.choice([10, 33, 80, 0, 1e9])}, {"height": r.choice([7, 50])}, {"width": 20, "height": 30, "resample": r.choice(["bicubic", "nearest", "bilinear", "lanczos", "automatic", "preserveDetails", "bicubicSharper", "none", "zz"])}, {"resolution": r.choice([150, 0, 1e6]), "resample": "none"}])})
    add("image", 2, lambda: {"cmd": "image.canvasSize", "params": r.choice([{}, {"width": r.choice([20, 80, 120]), "height": r.choice([10, 60]), "anchor": r.choice(["topLeft", "center", "bottomRight", "left", "top", "zz"])}, {"width": 10, "height": -5, "relative": True, "extensionColor": r.choice(["background", "foreground", "white", "black", "transparent", "#ff0080", "#xyz"])}])})
    add("image", 2, lambda: {"cmd": "image.crop", "params": r.choice([{}, {"x": r.randint(-10, 20), "y": r.randint(-10, 20), "width": r.randint(-2, 60), "height": r.randint(1, 50)}, {"x": 2, "y": 3, "width": 20, "height": 15, "deleteCroppedPixels": r.choice([True, False])}, {"x": 5, "y": 5, "width": 20, "height": 10, "angle": r.choice([15, -30, 360, 90, "x", 4000])}, {"x": 0, "y": 0, "width": 10, "height": 10, "targetWidth": r.choice([20, 5]), "targetHeight": 8, "resolution": r.choice([300, 0]), "resample": r.choice(["bilinear", "none", 3])}, {"x": 2147483640, "y": 0, "width": 100, "height": 5}])})
    add("image", 1, lambda: {"cmd": "image.trim", "params": r.choice([{}, {"basedOn": r.choice(["transparent", "topLeft", "bottomRight"]), "left": r.choice([True, False])}])})
    add("image", 1, lambda: {"cmd": r.choice(["image.mode.bits8", "image.mode.bits16", "image.mode.bits32", "image.duplicate"]), "params": r.choice([{}, {"name": "Dup"}, {"mergedOnly": True}])})
    add("image", 1, lambda: {"cmd": "image.rotation.arbitrary", "params": {"angle": r.choice([0, 15, 45, 90, -30, 400, 5000, "x"]), "direction": r.choice(["cw", "ccw", "q"]), **({"interpolation": r.choice(["nearest", "bilinear", "bicubic"])} if r.random() < 0.5 else {})}})
    add("image", 1, lambda: {"cmd": r.choice(["image.mode.indexedColor", "image.mode.bitmap", "image.mode.duotone", "image.mode.colorTable", "image.mode.grayscale"]), "params": r.choice([{}, {"palette": r.choice(["selective", "web", "uniform", "exact", "adaptive", "zz"]), "colors": r.choice([2, 16, 256, 300, 2.5]), "dither": r.choice(["none", "diffusion", "pattern", "noise"]), "forced": r.choice(["none", "primaries", "web"]), "transparency": r.choice([True, False])}, {"method": r.choice(["threshold", "pattern", "diffusion", "halftone", "zz"]), "frequency": 20, "shape": r.choice(["round", "line", "cross", "q"])}, {"type": r.choice(["monotone", "duotone", "tritone", "quadtone", "x"]), "inks": r.choice([[{"name": "A", "color": "#ff0000", "curve": [[0, 0], [100, 80]]}], 5])}, {"table": r.choice(["custom", "grayscale", "spectrum", "blackBody", "web", "systemMac", "zz"]), "entries": {"0": "#00ff00", "3": [1, 2, 3]}, "transparent": r.choice([None, 2, 999])}])})
    add("image", 2, lambda: {"cmd": "edit.transform", "params": r.choice([{}, {"matrix": [1, 0, 0, 1, r.randint(-10, 10), r.randint(-10, 10)]}, {"matrix": [0.5, 0, 0, 0.5, 0, 0], "interpolation": r.choice(["nearest", "bilinear"])}, {"quad": [[0, 0], [30, 2], [28, 25], [1, 20]]}, {"quad": [[0, 0], [0, 0], [0, 0], [0, 0]]}, {"matrix": [1, 0.3, 0, 1, 0, 0], "target": r.choice(["mask", "quickMask", {"channel": 0}])}, {"rect": [0, 0, 10, 10], "matrix": [2, 0, 0, 2, 0, 0]}])})
    def pt(r):
        return [r.choice([0, 3, 10.5, 30, -1, 200]), r.choice([0, 5, 12, 39.9])]
    add("select", 2, lambda: {"cmd": "select.magicWand", "params": {"x": r.randint(-2, 60), "y": r.randint(0, 40), **({"tolerance": r.choice([0, 10, 32, 255])} if r.random() < 0.5 else {}), **({"contiguous": False} if r.random() < 0.3 else {}), **({"antiAlias": False} if r.random() < 0.3 else {}), **({"sampleAllLayers": True} if r.random() < 0.3 else {}), **({"mode": r.choice(["add", "subtract", "intersect", "replace"])} if r.random() < 0.4 else {})}})
    add("select", 2, lambda: {"cmd": "select.colorRange", "params": r.choice([{}, {"select": r.choice(["reds", "yellows", "greens", "cyans", "blues", "magentas", "highlights", "midtones", "shadows", "outOfGamut", "zz", 3])}, {"color": color(r), "fuzziness": r.choice([0, 40, 200, -5])}, {"points": [pt(r), pt(r)], "subtractPoints": r.choice([[], [pt(r)]]), **({"order": r.choice([["+", "-", "+"], ["-", "+", "+"], ["x"]])} if r.random() < 0.3 else {}), "localized": r.choice([False, True]), "range": r.choice([50, 100])}, {"select": "midtones", "tonalRange": r.choice([[100, 200], [200, 100], 50, "x"])}, {"select": "shadows", "tonalRange": r.choice([40, 300])}, {"colors": ["#ff0000", "#00ff00"], "invert": True, "mode": r.choice(["add", "intersect", "replace"])}, {"points": "bad"}])})
    add("select", 2, lambda: {"cmd": "select.modify." + r.choice(["border", "smooth", "expand", "contract", "feather"]), "params": {"radius": r.choice([1, 2.5, 5, 0, -3, 1000, 0.1]), **({"applyAtCanvasBounds": True} if r.random() < 0.4 else {})}})
    add("select", 1, lambda: {"cmd": r.choice(["select.grow", "select.similar"]), "params": r.choice([{}, {"tolerance": r.choice([0, 64])}, {"sampleAllLayers": True}])})
    add("select", 1, lambda: {"cmd": "select.lasso", "params": {"points": r.choice([[[1, 1], [30, 2], [20, 25]], [[0, 0], [10, 0]], [[5.5, 5.5], [40, 10], [35, 30], [2, 28]], "x"]), **({"feather": r.choice([0, 2, 2000])} if r.random() < 0.4 else {}), **({"mode": r.choice(["add", "subtract"])} if r.random() < 0.3 else {}), **({"antiAlias": False} if r.random() < 0.3 else {})}})
    def vpath(r):
        return r.choice([
            {"subpaths": [{"closed": True, "knots": [[2, 2], [30, 4], [25, 28], [3, 20]]}]},
            {"subpaths": [{"closed": r.choice([True, False]), "op": r.choice(["combine", "subtract", "intersect", "exclude", "xor", "zz"]), "knots": [{"anchor": [10, 10], "in": [5, 12], "out": [15, 8], "smooth": True}, [30, 15], {"anchor": [20, 30]}]}], "fillRule": r.choice(["evenodd", "nonzero"]), "inverted": r.choice([False, True])},
            {"subpaths": [{"knots": [[1, 1], [8, 1], [8, 8]]}, {"op": "subtract", "knots": [[3, 3], [6, 3], [6, 6]]}]},
            {"subpaths": [{"knots": [["x", 1]]}]}, {"nope": 1}, 5])
    def vfill(r):
        return r.choice(["#ff0000", "#00ff0080", [0.1, 0.2, 0.3, 0.5], None, {"gradient": {"stops": [[0, "#000000"], [1, "#ffffff"]], "angle": 45, "scale": 80, "style": r.choice(["linear", "radial", "diamond", "q"]), "reverse": True}}, {"gradient": {}}, {"pattern": "Bricks", "scale": 50}, "#zz", 7])
    def vstroke(r):
        return r.choice([None, False, {"width": r.choice([1, 3, 2000000, -1]), "color": r.choice(["#0000ff", None]), "opacity": r.choice([50, 150]), "align": r.choice(["inside", "center", "outside", "q", 3]), "cap": r.choice(["butt", "round", "square"]), "join": r.choice(["miter", "round", "bevel"]), "dashes": r.choice([[2, 1], [], None, [20000], "x"])}, {"width": 2}, 5])
    def vrect(r):
        return [r.randint(-5, 20), r.randint(-5, 20), r.randint(0, 30), r.randint(0, 25)]
    add("vector", 4, lambda: {"cmd": "shape.create", "params": dict({"kind": r.choice(["rect", "roundedRect", "ellipse", "polygon", "star", "line", "path", "blob"]), "rect": vrect(r), "from": [2, 3], "to": [30, r.randint(0, 30)]}, **({"path": vpath(r)} if r.random() < 0.4 else {}), **({"fill": vfill(r)} if r.random() < 0.5 else {}), **({"stroke": vstroke(r)} if r.random() < 0.4 else {}), **({"radii": r.choice([3, [1, 2, 3, 4], "x"])} if r.random() < 0.3 else {}), **({"sides": r.choice([3, 7, 1000])} if r.random() < 0.3 else {}), **({"starRatio": r.choice([0.3, 2])} if r.random() < 0.3 else {}), **({"weight": r.choice([1, 4, -2])} if r.random() < 0.3 else {}), **({"addTo": layer_id(r), "op": r.choice(["subtract", "intersect", 3, "zz"])} if r.random() < 0.15 else {}))})
    add("vector", 3, lambda: {"cmd": "shape.edit", "params": r.choice([{"rect": vrect(r)}, {"kind": r.choice(["ellipse", "star", "rect", "line"]), "rect": vrect(r), "from": [0, 0], "to": [10, 10]}, {"move": [r.randint(-5, 5), 3]}, {"transform": r.choice([[2, 0, 0, 1, 0, 0], [0, 1, -1, 0, 30, 0], [1, 0.5, 0, 1, 0, 0], [-1, 0, 0, 1, 40, 0]])}, {"fill": vfill(r)}, {"stroke": vstroke(r)}, {"op": r.choice(["subtract", "exclude"]), **({"subpath": r.choice([0, 1, 9])} if r.random() < 0.5 else {})}, {"fillRule": "evenodd"}, {"path": vpath(r)}, {"name": "S"}, {"radii": 5}, {"sides": 6, "starRatio": 0.4}])})
    add("vector", 1, lambda: {"cmd": r.choice(["shape.info", "shape.rasterize", "layer.rasterize.shape", "path.list", "path.info", "select.toWorkPath", "select.convertToShape", "path.style.copyFill", "path.style.copyStroke", "path.style.pasteFill", "path.style.pasteStroke", "layer.vectorMask.info", "layer.rasterize.vectorMask", "layer.vectorMask.delete"]), "params": r.choice([{}, {"layer": layer_id(r)}, {"name": r.choice(["work", "layer", "P1", "zz"])}, {"tolerance": r.choice([1, 5])}])})
    add("vector", 2, lambda: {"cmd": "path.set", "params": {"path": vpath(r), **({"name": r.choice(["work", "P1", "P2", ""])} if r.random() < 0.6 else {}), **({"op": r.choice(["combine", "subtract", "zz"])} if r.random() < 0.3 else {})}})
    add("vector", 2, lambda: {"cmd": r.choice(["path.delete", "path.rename", "path.clippingPath.set", "path.clippingPath.clear"]), "params": {"name": r.choice(["work", "P1", "P2", None, ""]), "to": r.choice(["P2", "P3", "work", ""]), **({"flatness": r.choice([2, 200])} if r.random() < 0.3 else {})}})
    add("vector", 2, lambda: {"cmd": "path.transform", "params": dict({"name": r.choice(["work", "P1", "layer"])}, **r.choice([{"matrix": [1, 0, 0, 1, 5, 3]}, {"matrix": [1, 2]}, {"quad": [[0, 0], [30, 2], [28, 25], [1, 20]]}, {"quad": [[0, 0], [20, 0], [20, 10], [0, 10]], "rect": [0, 0, 20, 10]}, {"translateX": 4, "scaleY": 2, "angle": 30}, {}, {"angle": 1e9}]))})
    add("vector", 2, lambda: {"cmd": r.choice(["path.toSelection", "path.fill", "path.stroke"]), "params": {"name": r.choice(["work", "P1", "layer"]), **({"feather": r.choice([0, 2])} if r.random() < 0.3 else {}), **({"antiAlias": False} if r.random() < 0.3 else {}), **({"mode": r.choice(["add", "multiply", "screen"])} if r.random() < 0.3 else {}), **({"color": "#ff8800"} if r.random() < 0.3 else {}), **({"opacity": r.choice([50, 200])} if r.random() < 0.3 else {}), **({"tool": r.choice(["brush", "pencil", "eraser", "zz"])} if r.random() < 0.5 else {}), **({"simulatePressure": r.choice([True, "x"])} if r.random() < 0.3 else {}), **({"size": r.choice([3, 9000])} if r.random() < 0.3 else {})}})
    add("vector", 2, lambda: {"cmd": r.choice(["layer.vectorMask.add", "layer.vectorMask.fromPath", "layer.vectorMask.revealAll", "layer.vectorMask.hideAll", "layer.vectorMask.currentPath", "layer.vectorMask.edit", "layer.vectorMask.enabled", "layer.vectorMask.linked"]), "params": r.choice([{}, {"path": vpath(r)}, {"name": r.choice(["work", "P1"])}, {"hide": True}, {"density": 50, "feather": 3, "invert": True}, {"enabled": False}, {"linked": False}])})
    add("vector", 2, lambda: {"cmd": "layer.combineShapes." + r.choice(["unite", "subtractFrontShape", "intersectShapeAreas", "excludeOverlappingShapes", "mergeShapeComponents"]), "params": r.choice([{}, {"layer": layer_id(r)}, {"subpath": 0}])})
    add("select", 3, lambda: {"cmd": "select.float", "params": r.choice([{}, {"dx": r.randint(-8, 8), "dy": r.randint(-8, 8)}, {"dx": 3, "copy": True}, {"dx": 2.6, "dy": 1e12}, {"dy": -4, "target": r.choice(["mask", "pixels"])}])})
    add("select", 1, lambda: {"cmd": "select.drop"})
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
    areas = set(sys.argv[4].split(",")) if len(sys.argv) > 4 else {"select", "layer", "fill", "history", "image", "channel", "pattern", "misc", "edit", "extra", "color", "style", "adjust", "filter", "vector"}
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
