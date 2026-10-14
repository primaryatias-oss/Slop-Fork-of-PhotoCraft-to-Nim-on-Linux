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

import std/[json, options, strutils, macros, tables, math]
export json, options

template skipJson*() {.pragma.}
template jsonName*(name: string) {.pragma.}

type SerdeError* = object of ValueError

proc serdeError*(msg: string): ref SerdeError = newException(SerdeError, msg)

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

proc toJ*[T: object](x: T): JsonNode =
  mixin toJ
  result = newJObject()
  for name, v in x.fieldPairs:
    when v.hasCustomPragma(skipJson): discard
    elif v.hasCustomPragma(jsonName): result[v.getCustomPragmaVal(jsonName)] = toJ(v)
    else: result[fieldKey(T, name)] = toJ(v)

proc toJ*[T: ref object](x: T): JsonNode =
  mixin toJ
  if x.isNil: newJNull() else: toJ(x[])

# ---------------------------------------------------------------- from JSON

proc fromJ*(n: JsonNode; x: var bool) =
  if n.kind == JBool: x = n.getBool
  else: raise serdeError("expected a boolean")

proc fromJ*[T: SomeInteger](n: JsonNode; x: var T) =
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
  of JNull: x = T(NaN)
  else: raise serdeError("expected a number")

proc fromJ*(n: JsonNode; x: var string) =
  if n.kind == JString: x = n.getStr
  else: raise serdeError("expected a string")

proc fromJ*(n: JsonNode; x: var JsonNode) = x = n

proc fromJ*[T: enum](n: JsonNode; x: var T) =
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
  if n.kind != JArray: raise serdeError("expected an array")
  x.setLen(0)
  for e in n.elems:
    var v: T
    when compiles(defaultOf(T)): v = defaultOf(T)
    fromJ(e, v)
    x.add v

proc fromJ*[I, T](n: JsonNode; x: var array[I, T]) =
  mixin fromJ
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

proc fromJ*[T: object](n: JsonNode; x: var T) =
  mixin fromJ, defaultOf
  if n.kind != JObject: raise serdeError("expected an object for " & $T)
  for name, v in x.fieldPairs:
    when v.hasCustomPragma(skipJson): discard
    else:
      let key = when v.hasCustomPragma(jsonName): v.getCustomPragmaVal(jsonName) else: fieldKey(T, name)
      let e = n.getOrDefault(key)
      if not e.isNil:
        # A nested struct read from JSON starts from its own defaults (serde's `#[serde(default)]`),
        # not from a zeroed value.
        when v is object and compiles(defaultOf(typeof(v))):
          if e.kind == JObject: v = defaultOf(typeof(v))
        try: fromJ(e, v)
        except SerdeError as err: raise serdeError(key & ": " & err.msg)

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
    if n.len != 1: raise serdeError("expected a single-key enum object")
    for k, v in n.fields: return (k, v)
    raise serdeError("empty enum object")
  else: raise serdeError("expected an enum")

proc enumFromStr*[T: enum](s: string; _: typedesc[T]): Option[T] =
  for v in T:
    if $v == s: return some(v)
  none(T)
