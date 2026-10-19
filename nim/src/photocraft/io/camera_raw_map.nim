## Camera Raw Filter descriptors observed in real PSD files (filter version 18.4, process 6), port
## of `io/src/camera_raw_map.rs`. Verified develop controls are editable; other processing
## versions or settings keep the entire filter through the opaque-filter path.
##
## Also home of the descriptor helpers the smart-filter mapping shares (`bytesToHex`, `hexToBytes`,
## `blendOptions`, `setKey`, `rgbc`).

import std/[options, json, math, strutils, algorithm]
import ../color, ../psd
import ../algo/camera_raw
import ./blocks, ./effects_map

const
  CameraRawClass* = "Adobe Camera Raw Filter"
  CameraRawCommand* = "filter.cameraRaw"
  FilterId = 2783'i32
  TemplateKey* = "__cameraRawPsd"
  MaxTemplateBytes* = 1024 * 1024

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
    ## Exposure and sharpening radius are doubles; the other observed controls are integer levels.
  Arrays = [
    ("curveSplits", @["PC_1", "PC_2", "PC_3"]),
    ("hslHue", @["HA_R", "HA_O", "HA_Y", "HA_G", "HA_A", "HA_B", "HA_P", "HA_M"]),
    ("hslSat", @["SA_R", "SA_O", "SA_Y", "SA_G", "SA_A", "SA_B", "SA_P", "SA_M"]),
    ("hslLum", @["LA_R", "LA_O", "LA_Y", "LA_G", "LA_A", "LA_B", "LA_P", "LA_M"])]
  Curves = [("pointCurve", "Crv "), ("pointCurveRed", "CrvR"), ("pointCurveGreen", "CrvG"), ("pointCurveBlue", "CrvB")]
  NeutralOnly = [("LNRC", 0'i32), ("CNRS", 50'i32), ("MDis", 0'i32), ("VigA", 0'i32), ("VigM", 50'i32), ("crfs", 100'i32),
                 ("TMMs", 0'i32), ("PGTM", 0'i32), ("RGBt", 0'i32)]
    ## Controls with no PhotoCraft counterpart yet: their observed neutral values can stay in the
    ## template; active values keep the entire filter opaque.

# ---------------------------------------------------------------- shared descriptor helpers

proc bytesToHex*(bytes: openArray[byte]): string =
  ## Lowercase hex of `bytes`.
  const digits = "0123456789abcdef"
  for b in bytes:
    result.add digits[int(b shr 4)]
    result.add digits[int(b and 15)]

proc hexToBytes*(s: string): Option[seq[byte]] =
  if s.len mod 2 != 0: return none(seq[byte])
  var outp = newSeq[byte](s.len div 2)
  for i in countup(0, s.len - 2, 2):
    if s[i] notin HexDigits or s[i + 1] notin HexDigits: return none(seq[byte])
    outp[i div 2] = byte(parseHexInt(s[i .. i + 1]))
  some(outp)

proc blendOptions*(opacity0: float32; blend: BlendMode): Value =
  let opacity = (if opacity0.classify notin {fcNan, fcInf, fcNegInf}: float64(clamp(opacity0, 0'f32, 1'f32)) * 100 else: 100.0)
  vDesc(newDescriptor("blendOptions").with("Opct", vUnit("#Prc", opacity)).with("Md  ", blendLongValue(blend)))

proc setKey*(d: var Descriptor; key: string; v: Value) =
  ## Replaces `key`'s value, or appends it.
  for e in d.items.mitems:
    if e[0].s == key:
      e[1] = v
      return
  d.items.add (id(key), v)

proc rgbc*(v: float64): Value =
  vDesc(newDescriptor("RGBC").with("Rd  ", vDouble(v)).with("Grn ", vDouble(v)).with("Bl  ", vDouble(v)))

proc intAt(d: Descriptor; key: string): Option[int32] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkInteger: some(p.i) else: none(int32)

# ---------------------------------------------------------------- reading

proc defaultParams(): JsonNode = toJ(defaultCameraRaw())

proc knownKey(key: string): bool =
  if key in ["CrVe", "PrVN", "PrVe", "WBal"]: return true
  for (_, k) in Scalars:
    if k == key: return true
  for (_, keys) in Arrays:
    if key in keys: return true
  for (_, k) in Curves:
    if k == key: return true
  for (k, _) in NeutralOnly:
    if k == key: return true
  false

proc getParam(params: JsonNode; name: string): JsonNode =
  ## The parameter `name` (`wheel.field` for grading wheels), or nil.
  if params.isNil or params.kind != JObject: return nil
  let dot = name.find('.')
  if dot >= 0:
    let w = params{name[0 ..< dot]}
    if w.isNil or w.kind != JObject: return nil
    return w{name[dot + 1 .. ^1]}
  params{name}

proc setParam(params: JsonNode; name: string; v: JsonNode): bool =
  let dot = name.find('.')
  if dot >= 0:
    let w = params{name[0 ..< dot]}
    if w.isNil or w.kind != JObject or not w.hasKey(name[dot + 1 .. ^1]): return false
    w[name[dot + 1 .. ^1]] = v
    return true
  if not params.hasKey(name): return false
  params[name] = v
  true

proc numberRange(key: string): (float64, float64) =
  case key
  of "Ex12": (-5.0, 5.0)
  of "ShpR": (0.5, 3.0)
  of "Shrp": (0.0, 150.0)
  of "STSH", "STHH", "CgMH", "CgGH": (0.0, 360.0)
  of "PC_1", "PC_2", "PC_3", "ShpD", "ShpM", "LNR ", "LNRD", "CNR ", "CNRD", "STSS", "STHS", "CgMS", "CgGS", "CgBl", "GRNA",
     "GRNS", "GRNF", "PCVM", "PCVF": (0.0, 100.0)
  else: (-100.0, 100.0)

proc inRange(v: float64; r: (float64, float64)): bool = v >= r[0] and v <= r[1]

proc readNumber(d: Descriptor; key: string): Option[float64] =
  let p = d.getPtr(key)
  if p == nil: return
  var value: float64
  if key in ["Ex12", "ShpR"]:
    if p.kind != vkDouble: return
    value = p.d
  elif p.kind == vkInteger: value = float64(p.i)
  else: return
  if value == value and abs(value) != Inf and inRange(value, numberRange(key)): result = some(value)

proc curveJson(points: openArray[array[2, float32]]): JsonNode =
  result = newJArray()
  for q in points:
    var a = newJArray()
    for v in q: a.add newJFloat(float64(v))
    result.add a

proc readCurve(v: Value): Option[JsonNode] =
  if v.kind != vkList: return
  let values = v.list
  if values.len < 4 or values.len mod 2 != 0 or values.len > MaxCurvePoints * 2: return
  var points: seq[array[2, float32]]
  for i in countup(0, values.len - 2, 2):
    if values[i].kind != vkInteger or values[i + 1].kind != vkInteger: return
    points.add [float32(values[i].i), float32(values[i + 1].i)]
  try: validateCurve(points)
  except ValueError: return
  if points == @[[0'f32, 0], [255'f32, 255]]: return some(newJArray())
  some(curveJson(points))

proc validateSplits(params: JsonNode): string =
  ## "" when `curveSplits` holds three strictly increasing finite percentages, else the error.
  let values = params{"curveSplits"}
  if values.isNil or values.kind != JArray or values.len != 3: return "curveSplits must contain three numbers"
  var previous = -1.0
  for v in values:
    if v.kind notin {JInt, JFloat}: return "curve splits must be finite percentages"
    let x = v.getFloat
    if not (x == x and abs(x) != Inf and x >= 0.0 and x <= 100.0): return "curve splits must be finite percentages"
    if x <= previous: return "curve splits must be strictly increasing"
    previous = x
  ""

proc descriptorParams(d: Descriptor): Option[JsonNode] =
  if d.classId.s != CameraRawClass: return
  let (prvn, prve) = (intAt(d, "PrVN"), intAt(d, "PrVe"))
  if not ((prvn == some(5'i32) and prve == some(184549376'i32)) or (prvn == some(6'i32) and prve == some(251920384'i32))):
    return
  let crve = d.getPtr("CrVe")
  if crve == nil or crve.kind != vkText: return
  # A partial decode would omit active processing (masks, profiles, HDR or other settings) when
  # the layer is re-rendered: keep such a filter opaque instead.
  for (key, _) in d.items:
    if not knownKey(key.s): return
  let wbal = d.getPtr("WBal")
  if wbal != nil and not (wbal.kind == vkEnumerated and wbal.enumType.s == "WBal" and wbal.enumValue.s == "Cst "): return
  for (key, neutral) in NeutralOnly:
    if d.getPtr(key) != nil and intAt(d, key) != some(neutral): return
  var params = defaultParams()
  for (param, key) in Scalars:
    if d.getPtr(key) != nil:
      let v = readNumber(d, key)
      if v.isNone or not setParam(params, param, newJFloat(v.get)): return
  for (param, key) in Curves:
    let p = d.getPtr(key)
    if p != nil:
      let c = readCurve(p[])
      if c.isNone or not params.hasKey(param): return
      params[param] = c.get
  for (param, keys) in Arrays:
    for index, key in keys:
      if d.getPtr(key) != nil:
        let v = readNumber(d, key)
        let arr = params{param}
        if v.isNone or arr.isNil or arr.kind != JArray or index >= arr.len: return
        arr.elems[index] = newJFloat(v.get)
  if validateSplits(params).len > 0: return
  # The editor's own f32 representation, so opening and confirming without changes does not
  # rewrite a stored value.
  try: result = some(toJ(cameraRawFromJson(params)))
  except ValueError: discard

proc importParams*(item: Descriptor): Option[JsonNode] =
  ## The editable Camera Raw settings of a smart filter item (plus the item as a template), or
  ## none when the filter has to stay opaque.
  if intAt(item, "filterID") != some(FilterId): return
  let fl = getD(item, "Fltr")
  if fl.isNone: return
  let params = descriptorParams(fl.get)
  if params.isNone: return
  var tmpl = item
  setKey(tmpl, "blendOptions", blendOptions(1'f32, bmNormal))
  setKey(tmpl, "enab", vBool(true))
  let bytes = tmpl.toBytes
  if bytes.len > MaxTemplateBytes: return
  result = params
  result.get[TemplateKey] = %bytesToHex(bytes)

# ---------------------------------------------------------------- writing

proc supportedParam(key: string): bool =
  for (p, _) in Scalars:
    if p.split('.')[0] == key: return true
  for (p, _) in Arrays:
    if p == key: return true
  for (p, _) in Curves:
    if p == key: return true
  false

proc writeNumber(d: var Descriptor; key: string; value: JsonNode) =
  if value.isNil or value.kind notin {JInt, JFloat}: raise newException(ValueError, "Camera Raw " & key & " must be finite")
  let number = value.getFloat
  if not (number == number and abs(number) != Inf): raise newException(ValueError, "Camera Raw " & key & " must be finite")
  if not inRange(number, numberRange(key)):
    raise newException(ValueError, "Camera Raw " & key & " is outside the supported range")
  # PSD stores these as integer slider levels; PhotoCraft's sliders are continuous. Rounding (at
  # most half a level) keeps the filter instead of leaving it out of the PSD. Hue 360 is 0.
  let v = case key
    of "Ex12", "ShpR": vDouble(number)
    of "STSH", "STHH", "CgMH", "CgGH": vInt(int32(round(number)) mod 360)
    else: vInt(int32(round(number)))
  setKey(d, key, v)

proc sortedKeys(o: JsonNode): seq[string] =
  ## serde_json's map order (keys sorted).
  for k, _ in o: result.add k
  result.sort()

proc exportItem*(params: JsonNode): Descriptor =
  ## The `filterFXList` item for Camera Raw settings; raises `ValueError` when they have no
  ## verified PSD mapping.
  if params.isNil or params.kind != JObject: raise newException(ValueError, "Camera Raw parameters must be an object")
  let defaults = defaultParams()
  for key in sortedKeys(params):
    if key != "layer" and not key.startsWith("__") and not defaults.hasKey(key):
      raise newException(ValueError, "Camera Raw setting `" & key & "` has no verified PSD mapping")
  for wheel in ["gradeShadows", "gradeMidtones", "gradeHighlights", "gradeGlobal"]:
    let w = params{wheel}
    if not w.isNil and w.kind == JObject:
      for k, _ in w:
        if k notin ["hue", "sat", "lum"]:
          raise newException(ValueError, "Camera Raw `" & wheel & "` contains an unknown wheel setting")
  # Bound variable-sized curves before deserialization allocates their point arrays.
  for (param, _) in Curves:
    let c = params{param}
    if not c.isNil and c.kind == JArray and c.len > MaxCurvePoints:
      raise newException(ValueError, "Camera Raw `" & param & "` has too many points")
  var item: Descriptor
  let raw = params{TemplateKey}
  if not raw.isNil:
    if raw.kind != JString: raise newException(ValueError, "Camera Raw PSD template must be hex text")
    if raw.getStr.len > MaxTemplateBytes * 2: raise newException(ValueError, "Camera Raw PSD template is too large")
    let bytes = hexToBytes(raw.getStr)
    if bytes.isNone: raise newException(ValueError, "Camera Raw PSD template is not valid hex")
    try: item = descriptorFromBytes(bytes.get)
    except CatchableError as e: raise newException(ValueError, "bad Camera Raw PSD template: " & e.msg)
    let fl = getD(item, "Fltr")
    if item.classId.s != "filterFX" or intAt(item, "filterID") != some(FilterId) or fl.isNone or
        descriptorParams(fl.get).isNone:
      raise newException(ValueError, "Camera Raw PSD template is not a supported Camera Raw filter")
  else:
    item = newDescriptor("filterFX").with("Nm  ", Value(kind: vkText, text: ustrNul("Camera Raw Filter")))
      .with("hasoptions", vBool(true)).with("FrgC", rgbc(0)).with("BckC", rgbc(255))
      .with("Fltr", vDesc(newDescriptor(CameraRawClass).with("CrVe", Value(kind: vkText, text: ustrNul("18.4")))
        .with("PrVN", vInt(6)).with("PrVe", vInt(251920384))))
      .with("filterID", vInt(FilterId))
  var typed: CameraRaw
  try: typed = cameraRawFromJson(params)
  except ValueError as e: raise newException(ValueError, "bad Camera Raw parameters: " & e.msg)
  typed.validate()
  let normalized = toJ(typed)
  for key in sortedKeys(normalized):
    if not supportedParam(key) and defaults{key} != normalized[key]:
      raise newException(ValueError, "Camera Raw setting `" & key & "` has no verified PSD mapping")
  let splits = validateSplits(normalized)
  if splits.len > 0: raise newException(ValueError, splits)
  let fl = getD(item, "Fltr")
  if fl.isNone: raise newException(ValueError, "Camera Raw PSD template has no filter settings")
  var d = fl.get
  let original0 = descriptorParams(d)
  if original0.isNone: raise newException(ValueError, "unsupported Camera Raw processing version or settings")
  let original = original0.get
  for (param, key) in Scalars:
    let value = getParam(normalized, param)
    if value.isNil: raise newException(ValueError, "missing Camera Raw parameter")
    if getParam(original, param) != value:
      writeNumber(d, key, value)
      if key in ["Temp", "Tint"]: setKey(d, "WBal", vEnum("WBal", "Cst "))
  for (param, key) in Curves:
    let value = normalized{param}
    if value.isNil: raise newException(ValueError, "missing Camera Raw curve")
    if original{param} != value:
      if value.kind != JArray: raise newException(ValueError, "Camera Raw curve must be an array")
      var levels: seq[array[2, float32]]
      for point in value:
        if point.kind != JArray or point.len != 2:
          raise newException(ValueError, "Camera Raw curve point must contain two coordinates")
        var level: array[2, float32]
        for i in 0 .. 1:
          let c = point[i]
          let x = if c.kind in {JInt, JFloat}: c.getFloat else: NaN
          if not (x == x and x >= 0.0 and x <= 255.0):
            raise newException(ValueError, "Camera Raw curve coordinates must be levels 0–255")
          level[i] = float32(round(x))
        levels.add level
      if levels.len == 0: levels = @[[0'f32, 0], [255'f32, 255]]
      try: validateCurve(levels)
      except ValueError as e: raise newException(ValueError, "Camera Raw `" & param & "`: " & e.msg)
      var values: seq[Value]
      for l in levels:
        for v in l: values.add vInt(int32(v))
      setKey(d, key, vList(values))
  for (param, keys) in Arrays:
    for index, key in keys:
      let arr = normalized{param}
      if arr.isNil or arr.kind != JArray or index >= arr.len:
        raise newException(ValueError, "missing Camera Raw array parameter")
      let value = arr[index]
      let orig = original{param}
      let ov = if orig.isNil or orig.kind != JArray or index >= orig.len: nil else: orig[index]
      if ov != value: writeNumber(d, key, value)
  setKey(item, "Fltr", vDesc(d))
  item
