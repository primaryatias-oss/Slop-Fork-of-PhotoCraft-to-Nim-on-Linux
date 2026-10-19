## Camera Raw smart filters in PSD files (`io/src/camera_raw_map.rs` tests): verified develop
## controls round-trip and only edited fields are rewritten; unimplemented or malformed settings
## keep the filter opaque and lossless; exports of unsupported settings report errors.

import std/[unittest, options, json, strutils]
import photocraft/[color, doc, psd]
import photocraft/io/[camera_raw_map, smart_map, blocks]
import photocraft/algo/camera_raw

const
  Scalars = [
    ("temperature", "Temp"), ("tint", "Tint"), ("exposure", "Ex12"), ("contrast", "Cr12"), ("highlights", "Hi12"),
    ("shadows", "Sh12"), ("whites", "Wh12"), ("blacks", "Bk12"), ("texture", "CrTx"), ("clarity", "Cl12"),
    ("dehaze", "Dhze"), ("vibrance", "Vibr"), ("saturation", "Strt"), ("curveShadows", "PC_S"), ("curveDarks", "PC_D"),
    ("curveLights", "PC_L"), ("curveHighlights", "PC_H"), ("sharpenAmount", "Shrp"), ("sharpenRadius", "ShpR"),
    ("sharpenDetail", "ShpD"), ("sharpenMasking", "ShpM"), ("noiseLuminance", "LNR "), ("noiseLuminanceDetail", "LNRD"),
    ("noiseColor", "CNR "), ("noiseColorDetail", "CNRD"), ("gradeShadows.hue", "STSH"), ("gradeShadows.sat", "STSS"),
    ("gradeShadows.lum", "CgSL"), ("gradeMidtones.hue", "CgMH"), ("gradeMidtones.sat", "CgMS"),
    ("gradeMidtones.lum", "CgML"), ("gradeHighlights.hue", "STHH"), ("gradeHighlights.sat", "STHS"),
    ("gradeHighlights.lum", "CgHL"), ("gradeGlobal.hue", "CgGH"), ("gradeGlobal.sat", "CgGS"), ("gradeGlobal.lum", "CgGL"),
    ("gradeBlending", "CgBl"), ("gradeBalance", "STB "), ("grainAmount", "GRNA"), ("grainSize", "GRNS"),
    ("grainRoughness", "GRNF"), ("vignetteAmount", "PCVA"), ("vignetteMidpoint", "PCVM"), ("vignetteFeather", "PCVF"),
    ("vignetteRoundness", "PCVR")]
  Arrays = [
    ("curveSplits", @["PC_1", "PC_2", "PC_3"]),
    ("hslHue", @["HA_R", "HA_O", "HA_Y", "HA_G", "HA_A", "HA_B", "HA_P", "HA_M"]),
    ("hslSat", @["SA_R", "SA_O", "SA_Y", "SA_G", "SA_A", "SA_B", "SA_P", "SA_M"]),
    ("hslLum", @["LA_R", "LA_O", "LA_Y", "LA_G", "LA_A", "LA_B", "LA_P", "LA_M"])]
  Curves = [("pointCurve", "Crv "), ("pointCurveRed", "CrvR"), ("pointCurveGreen", "CrvG"), ("pointCurveBlue", "CrvB")]
  NeutralOnly = [("LNRC", 0'i32), ("CNRS", 50'i32), ("MDis", 0'i32), ("VigA", 0'i32), ("VigM", 50'i32), ("crfs", 100'i32),
                 ("TMMs", 0'i32), ("PGTM", 0'i32), ("RGBt", 0'i32)]

proc txt(s: string): Value = Value(kind: vkText, text: ustrNul(s))

proc getParam(p: JsonNode; name: string): JsonNode =
  let dot = name.find('.')
  if dot >= 0: p{name[0 ..< dot]}{name[dot + 1 .. ^1]} else: p{name}

proc setParam(p: JsonNode; name: string; v: JsonNode) =
  let dot = name.find('.')
  if dot >= 0: p[name[0 ..< dot]][name[dot + 1 .. ^1]] = v else: p[name] = v

proc fltrOf(item: Descriptor): Descriptor = getD(item, "Fltr").get

proc intList(v: openArray[int]): Value =
  var l: seq[Value]
  for x in v: l.add vInt(int32(x))
  vList(l)

proc sourceItem(): Descriptor =
  ## Parameter values observed in an 18.4 PSD (no pixels or private metadata).
  var d = newDescriptor(CameraRawClass).with("CrVe", txt("18.4")).with("PrVN", vInt(6)).with("PrVe", vInt(251920384))
    .with("Ex12", vDouble(1.15))
  for (key, value) in [("Cr12", -38), ("Hi12", 35), ("Sh12", -50), ("Wh12", 49), ("Bk12", -34), ("PC_S", 0), ("PC_D", -21),
                       ("PC_L", 38), ("PC_H", 0), ("PC_1", 25), ("PC_2", 50), ("PC_3", 75)]:
    d = d.with(key, vInt(int32(value)))
  for ai in 1 ..< Arrays.len:
    for key in Arrays[ai][1]:
      let value = case key
        of "HA_R": -50
        of "HA_Y": 29
        of "HA_G": 1
        else: 0
      d = d.with(key, vInt(int32(value)))
  newDescriptor("filterFX").with("Nm  ", txt("Camera Raw Filter")).with("blendOptions", blendOptions(1, bmNormal))
    .with("enab", vBool(true)).with("hasoptions", vBool(true)).with("FrgC", rgbc(0)).with("BckC", rgbc(255))
    .with("Fltr", vDesc(d)).with("filterID", vInt(2783))

proc allControlsItem(): Descriptor =
  result = sourceItem()
  var d = fltrOf(result)
  setKey(d, "WBal", vEnum("WBal", "Cst "))
  setKey(d, "ShpR", vDouble(1.2))
  for (key, value) in [("Temp", -26), ("Tint", -25), ("CrTx", -31), ("Cl12", 30), ("Dhze", -34), ("Vibr", 33), ("Strt", 33),
                       ("Shrp", 43), ("ShpD", 36), ("ShpM", 30), ("LNR ", 47), ("LNRD", 50), ("CNR ", 25), ("CNRD", 50),
                       ("STSH", 117), ("STSS", 52), ("STHH", 316), ("STHS", 70), ("STB ", 35), ("CgMH", 51), ("CgMS", 51),
                       ("CgSL", -24), ("CgML", 41), ("CgHL", 28), ("CgBl", 36), ("CgGH", 20), ("CgGS", 61), ("CgGL", 45),
                       ("GRNA", 45), ("GRNS", 25), ("GRNF", 50), ("PCVA", 31), ("PCVM", 50), ("PCVF", 50), ("PCVR", 0)]:
    setKey(d, key, vInt(int32(value)))
  for (key, neutral) in NeutralOnly: setKey(d, key, vInt(neutral))
  for (key, x, y) in [("Crv ", 146, 102), ("CrvR", 90, 174), ("CrvG", 162, 122), ("CrvB", 129, 202)]:
    setKey(d, key, intList([0, 0, x, y, 255, 255]))
  setKey(result, "Fltr", vDesc(d))

proc copyFilter(f: SmartFilter): SmartFilter =
  result = f
  result.params = f.params.copy

suite "io: Camera Raw smart filters":
  test "all observed develop controls round-trip and patch only the edited field":
    let source = allControlsItem()
    let f = filterFromItem(source)
    check f.command == CameraRawCommand
    check f.params["temperature"].getFloat == -26.0
    check f.params["texture"].getFloat == -31.0
    check abs(f.params["sharpenRadius"].getFloat - 1.2) < 1e-6
    check f.params["gradeHighlights"] == %*{"hue": 316.0, "sat": 70.0, "lum": 28.0}
    check f.params["pointCurveBlue"] == %*[[0.0, 0.0], [129.0, 202.0], [255.0, 255.0]]
    check itemForFilter(f) == source
    let defaults = toJ(defaultCameraRaw())
    for (param, key) in Scalars:
      var edited = copyFilter(f)
      var number = getParam(defaults, param).getFloat
      if getParam(f.params, param).getFloat == number: number += 1.0
      setParam(edited.params, param, newJFloat(number))
      var expected = fltrOf(source)
      setKey(expected, key, if key in ["Ex12", "ShpR"]: vDouble(number) else: vInt(int32(number)))
      check fltrOf(itemForFilter(edited)) == expected
    for (param, key) in Curves:
      var edited = copyFilter(f)
      edited.params[param] = newJArray()
      let item = itemForFilter(edited)
      var expected = fltrOf(source)
      setKey(expected, key, intList([0, 0, 255, 255]))
      check fltrOf(item) == expected
      check filterFromItem(item).params[param] == newJArray()

  test "active unimplemented controls and malformed curves stay opaque":
    for (key, neutral) in NeutralOnly:
      var item = allControlsItem()
      var d = fltrOf(item)
      setKey(d, key, vInt(neutral + 1))
      setKey(item, "Fltr", vDesc(d))
      let f = filterFromItem(item)
      check f.command == UnsupportedFilter
      check itemForFilter(f) == item
    for value in [intList([0]), intList([0, 0, 0, 255]), intList([0, 0, 255, 256]),
                  vList(@[vDouble(0), vDouble(0), vDouble(0), vDouble(0)]), intList(newSeq[int](34))]:
      var item = allControlsItem()
      var d = fltrOf(item)
      setKey(d, "Crv ", value)
      setKey(item, "Fltr", vDesc(d))
      let f = filterFromItem(item)
      check f.command == UnsupportedFilter
      check itemForFilter(f) == item

  test "the light curve and HSL are editable without rewriting unchanged settings":
    let source = sourceItem().with("futureOuterData", Value(kind: vkRawData, raw: @[1'u8, 2, 3, 4]))
    var f = filterFromItem(source)
    check f.command == CameraRawCommand
    check abs(f.params["exposure"].getFloat - 1.15) < 1e-6
    check f.params["contrast"].getFloat == -38.0
    check f.params["curveDarks"].getFloat == -21.0
    check f.params["curveLights"].getFloat == 38.0
    check f.params["curveSplits"] == %*[25.0, 50.0, 75.0]
    check f.params["hslHue"] == %*[-50.0, 0.0, 29.0, 1.0, 0.0, 0.0, 0.0, 0.0]
    check itemForFilter(f) == source
    f.params["exposure"] = newJFloat(0.0)
    f.params["hslHue"].elems[2] = newJFloat(-20.0)
    f.visible = false
    f.opacity = 0.4
    f.blend = bmMultiply
    let edited = itemForFilter(f)
    var expected = fltrOf(source)
    setKey(expected, "Ex12", vDouble(0))
    setKey(expected, "HA_Y", vInt(-20))
    check fltrOf(edited) == expected
    check edited.get("futureOuterData") == source.get("futureOuterData")
    let back = filterFromItem(edited)
    check (back.visible, back.blend) == (false, bmMultiply)
    check abs(back.opacity - 0.4'f32) < 1e-6
    check back.params["exposure"].getFloat == 0.0
    check back.params["hslHue"][2].getFloat == -20.0

  test "new filters write every verified control and accept explicit defaults":
    var params = toJ(defaultCameraRaw())
    params["seed"] = %0
    for (param, key) in Scalars:
      let number = case key
        of "Ex12": 0.75
        of "ShpR": 1.2
        else: 12.0
      setParam(params, param, newJFloat(number))
    for (param, keys) in Arrays:
      if param == "curveSplits": params[param] = %*[20, 45, 80]
      else:
        var a = newJArray()
        for i in 0 ..< keys.len: a.add newJFloat(float64(i) + 1.0)
        params[param] = a
    let f = SmartFilter(command: CameraRawCommand, params: params.copy, blend: bmScreen, opacity: 0.5, visible: false)
    let item = itemForFilter(f)
    let back = filterFromItem(descriptorFromBytes(item.toBytes))
    check (back.command, back.visible, back.blend, back.opacity) == (f.command, false, bmScreen, 0.5'f32)
    let expected = toJ(cameraRawFromJson(params))
    for (p, _) in Scalars: check getParam(back.params, p) == getParam(expected, p)
    for (p, _) in Arrays: check getParam(back.params, p) == getParam(expected, p)

  test "unsupported processing settings remain opaque and lossless":
    for (key, value) in [("LCs ", txt("unknown masks")), ("PrVN", vInt(7)), ("PrVe", vInt(123)), ("Ex12", vDouble(Inf)),
                         ("Cr12", txt("bad type"))]:
      var item = sourceItem()
      var d = fltrOf(item)
      setKey(d, key, value)
      setKey(item, "Fltr", vDesc(d))
      let f = filterFromItem(item)
      check f.command == UnsupportedFilter
      check itemForFilter(f) == item

  test "fractional slider levels round instead of dropping the filter":
    var f = filterFromItem(sourceItem())
    f.params["contrast"] = newJFloat(-12.4)
    f.params["hslHue"].elems[0] = newJFloat(10.5)
    f.params["gradeShadows"]["hue"] = newJFloat(359.7)
    f.params["pointCurve"] = %*[[0, 0], [100.2, 120.6], [200.6, 180.4], [255, 255]]
    let item = itemForFilter(f)
    let d = fltrOf(item)
    check d.get("Cr12") == some(vInt(-12))
    check d.get("HA_R") == some(vInt(11))
    check d.get("STSH") == some(vInt(0))
    check d.get("Crv ") == some(intList([0, 0, 100, 121, 201, 180, 255, 255]))
    let back = filterFromItem(item)
    check back.command == CameraRawCommand
    check back.params["contrast"].getFloat == -12.0

  test "unsupported or malformed exports report errors":
    var long = ""
    for _ in 0 .. MaxTemplateBytes * 2: long.add '0'
    for params in [newJNull(), %*{"exposure": "bright"}, %*{"exposure": 1e40}, %*{"exposure": 6}, %*{"contrast": 101},
                   %*{"hslHue": [1, 2]}, %*{"curveSplits": [80, 50, 20]}, %*{"curveSplits": [25, 25, 75]},
                   %*{"pointCurve": [[0, 0], [0, 255]]}, %*{"sharpenRadius": 4}, %*{"gradeShadows": {"hue": 361}},
                   %*{"gradeShadows": {"unknown": 1}}, %*{"seed": 1}, %*{"vignetteHighlights": 10}, %*{"futureSetting": 1},
                   %*{TemplateKey: "zz"}, %*{TemplateKey: "00"}, %*{TemplateKey: 12}, %*{TemplateKey: long}]:
      expect ValueError: discard exportItem(params)
    let wrong = newDescriptor("filterFX").with("filterID", vInt(1))
    expect ValueError: discard exportItem(%*{TemplateKey: bytesToHex(wrong.toBytes)})
