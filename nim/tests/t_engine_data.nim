## EngineData parse/write (port of `photocraft_text::engine_data` tests).

import std/[unittest, options]
import photocraft/text/engine_data

proc bytes(s: string): seq[byte] =
  for c in s: result.add byte(c)

const Sample = "\n\n<<\n\t/EngineDict\n\t<<\n\t\t/Editor\n\t\t<<\n\t\t\t/Text (\xFE\xFF\x00H\x00i\x00\\)\x00\r)\n\t\t>>\n\t\t/Nums [ 1.0 .5 -2 0 ]\n\t\t/Flag true\n\t\t/Kind /Roman\n\t\t/Runs [\n\t\t<<\n\t\t\t/A 1\n\t\t>>\n\t\t]\n\t>>\n>>"

suite "engine data":
  test "parses the sample":
    let v = parseEngineData(bytes(Sample))
    check v.path(["EngineDict", "Editor", "Text"]).asStr == some("Hi)\r")
    var nums: seq[float64]
    for x in v.path(["EngineDict", "Nums"]).asArray.get:
      if x.asF64.isSome: nums.add x.asF64.get
    check nums == @[1.0, 0.5, -2.0, 0.0]
    check v.path(["EngineDict", "Flag"]).asBool == some(true)
    check v.path(["EngineDict", "Kind"]) == some(edName("Roman"))
    check v.path(["EngineDict", "Runs"]).asArray.get[0].get("A").asI64 == some(1'i64)

  test "write/parse round trip is stable":
    let v = parseEngineData(bytes(Sample))
    let b = writeEngineData(v)
    check parseEngineData(b) == v
    check writeEngineData(parseEngineData(b)) == b

  test "reals format like Photoshop":
    check formatReal(1.0) == "1.0"
    check formatReal(0.5) == ".5"
    check formatReal(-0.25) == "-.25"
    check formatReal(12.125) == "12.125"
    check formatReal(0.015625) == ".01562"
    check formatReal(0.046875) == ".04688"
    check formatReal(-0.0) == "-0.0"
    check formatReal(1e15) == "1000000000000000."
    check formatReal(NaN) == "0.0"

  test "tiny reals round trip as numbers":
    for value in [0.000004, -0.000004]:
      check formatReal(value) == "0.0"
      let src = edDict(@[("EngineDict", edDict(@[("StyleRun", edArray(@[edDict(@[("BaselineShift", edReal(value))])]))]))])
      let parsed = parseEngineData(writeEngineData(src))
      check parsed.path(["EngineDict", "StyleRun"]).asArray.get[0].get("BaselineShift").asF64 == some(0.0)

  test "numbers parse like Rust":
    check parseRustF64("1.") == some(1.0)
    check parseRustF64(".5") == some(0.5)
    check parseRustF64("-.5e-2") == some(-0.005)
    check parseRustF64("1_0").isNone
    check parseRustF64(".").isNone
    check parseRustF64("e5").isNone
    check parseRustI64("+5") == some(5'i64)
    check parseRustI64("99999999999999999999").isNone
    check parseEngineData(bytes("99999999999999999999")) == edReal(1e20)
    check parseEngineData(bytes("-9223372036854775808")) == edInt(low(int64))

  test "malformed input never panics":
    let s = bytes(Sample)
    for cut in 0 ..< s.len:
      discard tryParseEngineData(s[0 ..< cut])
    check tryParseEngineData(bytes("<< /A")).isNone
    var deep = newSeq[byte](200)
    for b in deep.mitems: b = byte('[')
    check tryParseEngineData(deep).isNone
    check tryParseEngineData(bytes("<< /A 1 >>")).isSome
    check tryParseEngineData(bytes("\xFF")).isNone
