## Generic JSON (de)serialization with the shapes Rust's serde produces, so documents, settings
## and control-channel payloads stay interchangeable with the Rust build.
##
## * Objects: one JSON object; Nim camelCase field names map to serde's snake_case unless the
##   type opts into camelCase (`proc serdeCamel(T: typedesc[Foo]): bool = true`). Fields tagged
##   `{.skipJson.}` are left out. Missing fields keep the value already in the target (serde's
##   `#[serde(default)]`), unknown fields are ignored.
## * Enums: their string value (`$e`), as serde writes unit variants.
## * Tuples: JSON arrays. `Option[T]`: the value or `null`. Floats: NaN/±inf as `null`.
## * Rust enums with data are flat Nim objects with a `kind` field; their modules write
##   `toJ`/`fromJ` overloads using the helpers at the end (`tagged`, `untag`).

import std/[json, options, strutils, macros, tables, math, algorithm]
import rfmt
export json, options

template skipJson*() {.pragma.}
template jsonName*(name: string) {.pragma.}

type SerdeError* = object of ValueError

proc serdeError*(msg: string): ref SerdeError = newException(SerdeError, msg)

var serdeStrict* {.threadvar.}: bool
  ## While set, `fromJ` behaves like serde_json's `from_value`: errors carry serde's own text
  ## (`invalid type: string "x", expected f32`) with no key path, object keys are read in sorted
  ## order (serde_json maps are `BTreeMap`s, so the first bad key reported is the first in that
  ## order), and `null` is no float. Used where Rust reports serde's message (`withSerdeStrict`).

template withSerdeStrict*(body: untyped): untyped =
  let savedStrict = serdeStrict
  serdeStrict = true
  try: body
  finally: serdeStrict = savedStrict

proc serdeUnexpected*(v: JsonNode): string =
  ## serde's `Unexpected` text for a JSON value (`invalid type: <this>, expected …`).
  if v.isNil: return "null"
  case v.kind
  of JNull: "null"
  of JBool: "boolean `" & $v.bval & "`"
  of JInt: "integer `" & $v.num & "`"
  of JFloat:
    var t = displayFloat(v.fnum)
    if v.fnum == v.fnum and abs(v.fnum) != Inf and '.' notin t: t.add ".0"
    "floating point `" & t & "`"
  of JString: "string " & debugStr(v.str)
  of JArray: "sequence"
  of JObject: "map"

proc invalidType(v: JsonNode; expected: string): ref SerdeError =
  serdeError("invalid type: " & serdeUnexpected(v) & ", expected " & expected)

proc rustName(T: typedesc): string =
  ## The Rust primitive a Nim number type stands for, as serde names it in errors.
  when T is float32: "f32"
  elif T is float64: "f64"
  elif T is uint8: "u8"
  elif T is uint16: "u16"
  elif T is uint32: "u32"
  elif T is uint64 or T is uint: "u64"
  elif T is int8: "i8"
  elif T is int16: "i16"
  elif T is int32: "i32"
  else: "i64"

proc oneOf*(names: openArray[string]): string =
  ## serde's `OneOf` list of expected variants.
  case names.len
  of 0: "there are no variants"
  of 1: "expected `" & names[0] & "`"
  of 2: "expected `" & names[0] & "` or `" & names[1] & "`"
  else:
    var parts: seq[string]
    for n in names: parts.add "`" & n & "`"
    "expected one of " & parts.join(", ")

proc snakeCase*(s: string): string =
  for i, c in s:
    if c in {'A'..'Z'}:
      if i > 0: result.add '_'
      result.add char(ord(c) + 32)
    else: result.add c

proc camelFromSnake*(s: string): string =
  var up = false
  for c in s:
    if c == '_': up = true
    elif up:
      result.add c.toUpperAscii
      up = false
    else: result.add c

# ---------------------------------------------------------------- to JSON

proc toJ*(x: bool): JsonNode = newJBool(x)
proc toJ*(x: SomeSignedInt): JsonNode = newJInt(BiggestInt(x))
proc toJ*(x: SomeUnsignedInt): JsonNode =
  if uint64(x) > uint64(high(BiggestInt)): newJFloat(float(x)) else: newJInt(BiggestInt(x))
proc toJ*(x: SomeFloat): JsonNode =
  let f = float64(x)
  if f != f or f == Inf or f == NegInf: newJNull()
  elif x is float32: newJFloat(parseFloat($float32(x)))
  else: newJFloat(f)
proc toJ*(x: string): JsonNode = newJString(x)
proc toJ*(x: char): JsonNode = newJString($x)
proc toJ*[T: enum](x: T): JsonNode = newJString($x)
proc toJ*(x: JsonNode): JsonNode = (if x.isNil: newJNull() else: x)

proc toJ*[T](x: Option[T]): JsonNode =
  mixin toJ
  if x.isSome: toJ(x.get) else: newJNull()

proc toJ*[T](x: seq[T]): JsonNode =
  mixin toJ
  result = newJArray()
  for v in x: result.add toJ(v)

proc toJ*[I, T](x: array[I, T]): JsonNode =
  mixin toJ
  result = newJArray()
  for v in x: result.add toJ(v)

proc toJ*[K, V](x: OrderedTable[K, V]): JsonNode =
  mixin toJ
  result = newJObject()
  for k, v in x: result[$k] = toJ(v)

proc fieldKey(T: typedesc; name: string): string =
  mixin serdeCamel
  when compiles(serdeCamel(T)):
    if serdeCamel(T): name else: snakeCase(name)
  else: snakeCase(name)

proc toJ*[T: tuple](x: T): JsonNode =
  mixin toJ
  result = newJArray()
  for v in x.fields: result.add toJ(v)

proc objectToJ*[T: object](x: T): JsonNode =
  ## The generic struct form (for custom `toJ` overloads that adjust it).
  mixin toJ
  result = newJObject()
  for name, v in x.fieldPairs:
    when v.hasCustomPragma(skipJson): discard
    elif v.hasCustomPragma(jsonName): result[v.getCustomPragmaVal(jsonName)] = toJ(v)
    else: result[fieldKey(T, name)] = toJ(v)

proc toJ*[T: object](x: T): JsonNode = objectToJ(x)

proc toJ*[T: ref object](x: T): JsonNode =
  mixin toJ
  if x.isNil: newJNull() else: toJ(x[])

# ---------------------------------------------------------------- from JSON

proc fromJ*(n: JsonNode; x: var bool) =
  if n.kind == JBool: x = n.getBool
  elif serdeStrict: raise invalidType(n, "a boolean")
  else: raise serdeError("expected a boolean")

proc fromJ*[T: SomeInteger](n: JsonNode; x: var T) =
  if serdeStrict:
    case n.kind
    of JInt:
      let v = n.getBiggestInt
      when T is SomeUnsignedInt:
        if v < 0 or uint64(v) > uint64(high(T)): raise serdeError("invalid value: integer `" & $v & "`, expected " & rustName(T))
      else:
        if v < BiggestInt(low(T)) or v > BiggestInt(high(T)):
          raise serdeError("invalid value: integer `" & $v & "`, expected " & rustName(T))
      x = T(v)
    else: raise invalidType(n, rustName(T))
    return
  case n.kind
  of JInt:
    let v = n.getBiggestInt
    when T is SomeUnsignedInt:
      if v < 0: raise serdeError("expected an unsigned integer")
      if uint64(v) > uint64(high(T)): raise serdeError("integer out of range")
    else:
      if v < BiggestInt(low(T)) or v > BiggestInt(high(T)): raise serdeError("integer out of range")
    x = T(v)
  of JFloat:
    let f = n.getFloat
    if f != f or f != trunc(f): raise serdeError("expected an integer")
    if f < float(low(T)) or f > float(high(T)): raise serdeError("integer out of range")
    x = T(f)
  else: raise serdeError("expected an integer")

proc fromJ*[T: SomeFloat](n: JsonNode; x: var T) =
  case n.kind
  of JInt: x = T(n.getBiggestInt)
  of JFloat: x = T(n.getFloat)
  of JNull:
    if serdeStrict: raise invalidType(n, rustName(T))
    x = T(NaN)
  else:
    if serdeStrict: raise invalidType(n, rustName(T))
    raise serdeError("expected a number")

proc fromJ*(n: JsonNode; x: var string) =
  if n.kind == JString: x = n.getStr
  elif serdeStrict: raise invalidType(n, "a string")
  else: raise serdeError("expected a string")

proc fromJ*(n: JsonNode; x: var JsonNode) = x = n

proc fromJ*[T: enum](n: JsonNode; x: var T) =
  if serdeStrict:
    # serde_json's `deserialize_enum` on a `Value`: a string, or a map with a single key.
    var name: string
    var payload: JsonNode
    case n.kind
    of JString: name = n.getStr
    of JObject:
      if n.len != 1: raise serdeError("invalid value: map, expected map with a single key")
      for key, val in pairs(n.fields):
        name = key
        payload = val
    else: raise invalidType(n, "string or map")
    var names: seq[string]
    for v in T: names.add $v
    for v in T:
      if $v == name:
        if not payload.isNil and payload.kind != JNull: raise invalidType(payload, "unit")
        x = v
        return
    raise serdeError("unknown variant `" & name & "`, " & oneOf(names))
  if n.kind != JString: raise serdeError("expected a string for " & $T)
  let s = n.getStr
  for v in T:
    if $v == s:
      x = v
      return
  raise serdeError("unknown " & $T & " variant " & escape(s))

proc fromJ*[T](n: JsonNode; x: var Option[T]) =
  mixin fromJ
  if n.isNil or n.kind == JNull: x = none(T)
  else:
    var v: T
    when compiles(defaultOf(T)): v = defaultOf(T)
    fromJ(n, v)
    x = some(v)

proc fromJ*[T](n: JsonNode; x: var seq[T]) =
  mixin fromJ
  if n.kind != JArray:
    if serdeStrict: raise invalidType(n, "a sequence")
    raise serdeError("expected an array")
  x.setLen(0)
  for e in n.elems:
    var v: T
    when compiles(defaultOf(T)): v = defaultOf(T)
    fromJ(e, v)
    x.add v

proc fromJ*[I, T](n: JsonNode; x: var array[I, T]) =
  mixin fromJ
  if serdeStrict:
    # serde's `[T; N]` visitor reads element by element, then serde_json refuses extra elements.
    if n.kind != JArray: raise invalidType(n, "an array of length " & $x.len)
    var i = 0
    for k in low(x) .. high(x):
      if i >= n.len: raise serdeError("invalid length " & $n.len & ", expected an array of length " & $x.len)
      fromJ(n.elems[i], x[k])
      inc i
    if n.len > x.len: raise serdeError("invalid length " & $n.len & ", expected fewer elements in array")
    return
  if n.kind != JArray or n.len != x.len: raise serdeError("expected an array of " & $x.len)
  var i = 0
  for k in low(x) .. high(x):
    fromJ(n.elems[i], x[k])
    inc i

proc fromJ*[K, V](n: JsonNode; x: var OrderedTable[K, V]) =
  mixin fromJ
  if n.kind != JObject: raise serdeError("expected an object")
  x.clear()
  for k, e in n.fields:
    var v: V
    fromJ(e, v)
    x[k] = v

proc fromJ*[T: tuple](n: JsonNode; x: var T) =
  mixin fromJ
  if n.kind != JArray: raise serdeError("expected an array")
  var i = 0
  for v in x.fields:
    if i >= n.len: raise serdeError("tuple too short")
    fromJ(n.elems[i], v)
    inc i

proc serdeRequired*(T: typedesc): bool = false
  ## Whether the Rust struct `T` stands for lacks `#[serde(default)]`, so that in strict mode a
  ## missing field is an error (`missing field `x``). Types opt in with an overload.

proc objectFromJ*[T: object](n: JsonNode; x: var T) =
  ## The generic struct form (for custom `fromJ` overloads that adjust it).
  ## Like serde's derived `Deserialize`, a JSON array fills the fields in declaration order
  ## (missing trailing fields keep their defaults).
  mixin fromJ, defaultOf, serdeRequired
  if serdeStrict and n.kind == JObject:
    # serde_json visits a map's keys in sorted order; errors carry no key path.
    var keys: seq[string]
    for k in n.fields.keys: keys.add k
    keys.sort(cmp[string])
    for k in keys:
      let e = n.fields[k]
      for name, v in x.fieldPairs:
        when not v.hasCustomPragma(skipJson):
          let key = when v.hasCustomPragma(jsonName): v.getCustomPragmaVal(jsonName) else: fieldKey(T, name)
          if key == k:
            when v is object and compiles(defaultOf(typeof(v))):
              if e.kind in {JObject, JArray}: v = defaultOf(typeof(v))
            fromJ(e, v)
    when serdeRequired(T):
      for name, v in x.fieldPairs:
        when not v.hasCustomPragma(skipJson):
          let key = when v.hasCustomPragma(jsonName): v.getCustomPragmaVal(jsonName) else: fieldKey(T, name)
          when v isnot Option:
            if not n.fields.hasKey(key): raise serdeError("missing field `" & key & "`")
    return
  if n.kind notin {JObject, JArray}:
    if serdeStrict: raise invalidType(n, "struct " & $T)
    raise serdeError("expected an object for " & $T)
  var idx = 0
  for name, v in x.fieldPairs:
    when v.hasCustomPragma(skipJson): discard
    else:
      let key = when v.hasCustomPragma(jsonName): v.getCustomPragmaVal(jsonName) else: fieldKey(T, name)
      let e = if n.kind == JArray: (if idx < n.len: n.elems[idx] else: nil) else: n.getOrDefault(key)
      inc idx
      if not e.isNil:
        # A nested struct read from JSON starts from its own defaults (serde's `#[serde(default)]`),
        # not from a zeroed value.
        when v is object and compiles(defaultOf(typeof(v))):
          if e.kind in {JObject, JArray}: v = defaultOf(typeof(v))
        if serdeStrict: fromJ(e, v)
        else:
          try: fromJ(e, v)
          except SerdeError as err: raise serdeError(key & ": " & err.msg)
  if n.kind == JArray and n.len > idx:
    raise serdeError("invalid length " & $n.len & ", expected fewer elements" & (if serdeStrict: " in array" else: ""))

proc fromJ*[T: object](n: JsonNode; x: var T) = objectFromJ(n, x)

proc fromJ*[T: ref object](n: JsonNode; x: var T) =
  mixin fromJ
  if n.isNil or n.kind == JNull: x = nil
  else:
    if x.isNil: new(x)
    fromJ(n, x[])

proc fromJson*[T](n: JsonNode; _: typedesc[T]): T =
  mixin fromJ, defaultOf
  when compiles(defaultOf(T)): result = defaultOf(T)
  fromJ(n, result)

# ---------------------------------------------------------------- enum-with-data helpers

proc tagged*(tag: string; payload: JsonNode): JsonNode =
  ## Serde's externally tagged form: `{"Tag": payload}`.
  result = newJObject()
  result[tag] = payload

proc untag*(n: JsonNode): (string, JsonNode) =
  ## Splits `"Unit"` or `{"Tag": payload}` into the tag and payload (nil for unit variants).
  case n.kind
  of JString: (n.getStr, nil)
  of JObject:
    if n.len != 1:
      if serdeStrict: raise serdeError("invalid value: map, expected map with a single key")
      raise serdeError("expected a single-key enum object")
    for k, v in n.fields: return (k, v)
    raise serdeError("empty enum object")
  else:
    if serdeStrict: raise invalidType(n, "string or map")
    raise serdeError("expected an enum")

proc unitPayload*(payload: JsonNode) =
  ## serde_json's `unit_variant`: a unit variant written as a map takes only `null`.
  if not payload.isNil and payload.kind != JNull: raise invalidType(payload, "unit")

proc newtypePayload*(payload: JsonNode) =
  ## serde_json's `newtype_variant`: the variant needs its value.
  if payload.isNil: raise serdeError("invalid type: unit variant, expected newtype variant")

proc structPayload*(payload: JsonNode) =
  ## serde_json's `struct_variant`: the variant needs a map.
  if payload.isNil: raise serdeError("invalid type: unit variant, expected struct variant")
  if payload.kind != JObject: raise invalidType(payload, "struct variant")

proc enumFromStr*[T: enum](s: string; _: typedesc[T]): Option[T] =
  for v in T:
    if $v == s: return some(v)
  none(T)
