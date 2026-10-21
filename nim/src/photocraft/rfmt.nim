## Rust-compatible float formatting (`{:?}`, `{}`, `{:.N}` of `f32` and `f64`) and
## `slice::partition_point`, shared by the raw modules and the engine.

import std/[strutils, math]

template partitionPoint*[T](a: openArray[T]; pred: untyped): int =
  ## Rust `slice::partition_point` (its binary search, step for step); `pred` is an expression
  ## over `it`.
  block:
    var size = a.len
    var base = 0
    var res = 0
    if size > 0:
      while size > 1:
        let half = size div 2
        let mid = base + half
        let it {.inject.} = a[mid]
        if pred: base = mid
        size -= half
      let it {.inject.} = a[base]
      res = base + (if pred: 1 else: 0)
    res

proc fixed*(v: float64; prec: int): string =
  ## Rust `format!("{:.prec$}", v)`.
  if v != v: return "NaN"
  if v == Inf: return "inf"
  if v == NegInf: return "-inf"
  formatFloat(v, ffDecimal, prec)

proc digitsOf[T: SomeFloat](v: T): (string, int) =
  ## Shortest round-trip significant digits of `abs(v)` (finite, non-zero) and the decimal point
  ## position: value = 0.d₁d₂… × 10^point.
  let s = $abs(v)
  var mant = s
  var exp = 0
  let e = s.find({'e', 'E'})
  if e >= 0:
    mant = s[0 ..< e]
    exp = parseInt(s[e + 1 .. ^1].replace("+", ""))
  let dot = mant.find('.')
  let intPart = if dot < 0: mant else: mant[0 ..< dot]
  let frac = if dot < 0: "" else: mant[dot + 1 .. ^1]
  var digits = intPart & frac
  var point = intPart.len + exp
  while digits.len > 1 and digits[0] == '0':
    digits = digits[1 .. ^1]
    dec point
  while digits.len > 1 and digits[^1] == '0': digits.setLen(digits.len - 1)
  (digits, point)

proc debugFloat*[T: SomeFloat](v: T): string =
  ## Rust `{:?}` of an `f32` / `f64`: shortest round-trip digits (of the value's own type), `.0`
  ## on integers, exponent form below 1e-4 and from 1e16.
  if v != v: return "NaN"
  if v == Inf: return "inf"
  if v == NegInf: return "-inf"
  let sign = if signbit(v): "-" else: ""
  if v == 0: return sign & "0.0"
  let (digits, point) = digitsOf(v)
  let a = abs(v)
  if a < 1e-4 or a >= 1e16:
    result = sign & digits[0 .. 0]
    if digits.len > 1: result.add "." & digits[1 .. ^1]
    result.add "e" & $(point - 1)
  elif point <= 0:
    result = sign & "0." & repeat('0', -point) & digits
  elif point >= digits.len:
    result = sign & digits & repeat('0', point - digits.len) & ".0"
  else:
    result = sign & digits[0 ..< point] & "." & digits[point .. ^1]

proc displayFloat*[T: SomeFloat](v: T): string =
  ## Rust `{}` of an `f32` / `f64`: shortest round-trip digits, never an exponent, no `.0` on
  ## integers.
  if v != v: return "NaN"
  if v == Inf: return "inf"
  if v == NegInf: return "-inf"
  let sign = if signbit(v): "-" else: ""
  if v == 0: return sign & "0"
  let (digits, point) = digitsOf(v)
  if point <= 0: sign & "0." & repeat('0', -point) & digits
  elif point >= digits.len: sign & digits & repeat('0', point - digits.len)
  else: sign & digits[0 ..< point] & "." & digits[point .. ^1]

proc debugF64*(v: float64): string = debugFloat(v)
proc displayF64*(v: float64): string = displayFloat(v)

proc debugMat3*(m: array[3, array[3, float64]]): string =
  ## Rust `{:?}` of a `[[f64; 3]; 3]`.
  result = "["
  for i in 0 .. 2:
    if i > 0: result.add ", "
    result.add "["
    for j in 0 .. 2:
      if j > 0: result.add ", "
      result.add debugF64(m[i][j])
    result.add "]"
  result.add "]"
