## Document model tests: tree operations, ids, naming, and serde-compatible JSON.
import std/[unittest, json, options]
import photocraft/doc

suite "doc":
  test "tree operations":
    var d = withBackground("Untitled", size(64, 32), cmRgb, stU8, White)
    check d.layerCount == 1
    let bg = d.layers[0].id
    let a = d.insertAbove(some(bg), rasterLayer(d.nextLayerName("Layer"), d.pixelFormat))
    check d.layers[1].name == "Layer 1"
    var g = groupLayer("Group 1", @[rasterLayer("Inner", d.pixelFormat)])
    let innerId = g.content.group.children[0].id
    let gid = d.insertAbove(some(a), g)
    check d.layerCount == 4
    check d.pathOf(innerId) == some(@[2, 0])
    check d.layer(innerId).get.name == "Inner"
    check d.maxGroupDepth == 1
    check d.shift(a, 1)
    check d.layers[2].id == a
    check not d.shift(a, 1)
    check d.remove(gid).isSome
    check d.layerCount == 2
    check d.layers[0].locks.transparency
    check d.locksAt(@[0]).position
    var px: array[4, float32]
    d.layers[0].surface.readPixel(3, 3, px)
    check px == [1'f32, 1, 1, 1]

  test "names and duplicates":
    var d = newDocument("x", size(8, 8), cmRgb, stU8)
    discard d.insertAbove(none(LayerId), rasterLayer("Layer 1", d.pixelFormat))
    check d.copyName("Layer 1") == "Layer 1 copy"
    discard d.insertAbove(none(LayerId), rasterLayer("Layer 1 copy", d.pixelFormat))
    check d.copyName("Layer 1") == "Layer 1 copy 2"
    check d.copyName("Layer 1 copy") == "Layer 1 copy 2"
    let dup = d.layers[0].duplicate
    check dup.id != d.layers[0].id and dup.name == d.layers[0].name

  test "enum-like types serialize like serde":
    var lv = identityLevels()
    lv.levelsMaster.gamma = 1.5
    let j = toJ(lv)
    check j.hasKey("Levels") and j["Levels"]["master"]["gamma"].getFloat == 1.5
    check fromJson(j, Adjustment) == lv
    check toJ(newAdjustment(akInvert)) == newJString("Invert")
    let e = defaultDropShadow()
    check fromJson(toJ(e), Effect) == e
    let f = gradientFill(@[(0'f32, Black), (1'f32, White)], 90, 1, gsRadial, false)
    let fj = toJ(f)
    check fj["Gradient"]["style"].getStr == "Radial"
    check fromJson(fj, Fill) == f
    check toJ(Contour(kind: ckLinear)) == newJString("Linear")
    let adv = defaultAdvancedBlending()
    check toJ(adv).hasKey("blendClipped")

  test "text styles resolve and diff":
    var ts = defaultTextStyles()
    ts.character.add CharacterStyleDef(id: 1, name: "Bold", attrs: %*{"faux_bold": true, "size_pt": 20})
    let c = ts.resolveChar(none(uint32), some(1'u32), "Arial")
    check c.fauxBold and c.sizePt == 20 and c.fontFamily == "Arial" and c.styleSheet == some(1'u32)
    var run = c
    run.underline = true
    let ov = ts.charOverrides(run, none(uint32), "Arial")
    check ov.len == 1 and ov["underline"].getBool
    expect SerdeError: discard applyAttrs(defaultCharStyle(), %*{"nope": 1})
    # Styles without a sheet reference (no `style_sheet` key) diff without raising.
    var big = defaultCharStyle()
    big.sizePt = 30
    check diffAttrs(big, defaultCharStyle()) == %*{"size_pt": 30.0}
    check diffAttrs(defaultParagraphStyle(), defaultParagraphStyle()).len == 0

  test "auto slices cover the canvas":
    let s = autoSlices(rect(0, 0, 100, 100), [rect(10, 10, 50, 50)])
    var area = 0
    for r in s: area += r.width * r.height
    check area == 100 * 100 - 40 * 40
