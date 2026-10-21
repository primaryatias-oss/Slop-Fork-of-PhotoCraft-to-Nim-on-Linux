## JSON text read exactly as serde_json (without its `float_roundtrip` feature) reads it, for inputs
## that must give the Rust build's values bit for bit: command scripts, control-channel requests.
##
## The difference from `std/json` is the numbers. serde_json keeps integers as `u64`/`i64` and
## builds a decimal from its digits as `significand as f64` scaled by a power of ten, two roundings
## that can land one ULP from the correctly rounded value `std/json` gives; a parameter that is
## re-serialised (a stroke's points hashed into its jitter seed) or used in arithmetic then differs.

import std/[json, strutils, unicode]

var pow10: array[309, float64]
for i in 0 .. 308: pow10[i] = parseFloat("1e" & $i)

type
  SjsonError* = object of ValueError
  Parser = object
    s: string
    i: int

proc fail(msg: string): ref SjsonError = newException(SjsonError, msg)

proc peek(p: Parser): char = (if p.i < p.s.len: p.s[p.i] else: '\0')

proc skipWs(p: var Parser) =
  while p.i < p.s.len and p.s[p.i] in {' ', '\t', '\n', '\r'}: inc p.i

proc overflowU64(sig, digit: uint64): bool =
  ## serde_json's `overflow!(significand * 10 + digit, u64::MAX)`.
  const m = high(uint64)
  sig >= m div 10 and (sig > m div 10 or digit > m mod 10)

proc f64FromParts(positive: bool; significand: uint64; exponent0: int32): float64 =
  var f = float64(significand)
  var exponent = exponent0
  while true:
    let k = (if exponent == low(int32): int(high(int32)) + 1 else: abs(int(exponent)))
    if k < pow10.len:
      if exponent >= 0:
        f *= pow10[k]
        if f == Inf: raise fail("number out of range")
      else: f /= pow10[k]
      break
    if f == 0: break
    if exponent >= 0: raise fail("number out of range")
    f /= 1e308
    exponent += 308
  if positive: f else: -f

proc parseExponentOverflow(p: var Parser; positive, zeroSignificand, positiveExp: bool): float64 =
  if not zeroSignificand and positiveExp: raise fail("number out of range")
  while p.peek in {'0' .. '9'}: inc p.i
  if positive: 0.0 else: -0.0

proc parseExponent(p: var Parser; positive: bool; significand: uint64; startingExp: int32): float64 =
  inc p.i
  var positiveExp = true
  if p.peek == '+': inc p.i
  elif p.peek == '-':
    inc p.i
    positiveExp = false
  if p.peek notin {'0' .. '9'}: raise fail("invalid number")
  var exp = int32(ord(p.peek) - ord('0'))
  inc p.i
  while p.peek in {'0' .. '9'}:
    let digit = int32(ord(p.peek) - ord('0'))
    inc p.i
    if exp >= high(int32) div 10 and (exp > high(int32) div 10 or digit > high(int32) mod 10):
      return p.parseExponentOverflow(positive, significand == 0, positiveExp)
    exp = exp * 10 + digit
  let finalExp = (if positiveExp: int32(clamp(int64(startingExp) + exp, low(int32), high(int32)))
                  else: int32(clamp(int64(startingExp) - exp, low(int32), high(int32))))
  f64FromParts(positive, significand, finalExp)

proc parseDecimalOverflow(p: var Parser; positive: bool; significand: uint64; exponent: int32): float64 =
  while p.peek in {'0' .. '9'}: inc p.i
  if p.peek in {'e', 'E'}: p.parseExponent(positive, significand, exponent)
  else: f64FromParts(positive, significand, exponent)

proc parseDecimal(p: var Parser; positive: bool; significand0: uint64; before: int32): float64 =
  inc p.i
  var significand = significand0
  var after = 0'i32
  while p.peek in {'0' .. '9'}:
    let digit = uint64(ord(p.peek) - ord('0'))
    if overflowU64(significand, digit): return p.parseDecimalOverflow(positive, significand, before + after)
    inc p.i
    significand = significand * 10 + digit
    dec after
  if after == 0: raise fail("invalid number")
  if p.peek in {'e', 'E'}: p.parseExponent(positive, significand, before + after)
  else: f64FromParts(positive, significand, before + after)

proc parseLongInteger(p: var Parser; positive: bool; significand: uint64): float64 =
  var exponent = 0'i32
  while true:
    case p.peek
    of '0' .. '9':
      inc p.i
      inc exponent
    of '.': return p.parseDecimal(positive, significand, exponent)
    of 'e', 'E': return p.parseExponent(positive, significand, exponent)
    else: return f64FromParts(positive, significand, exponent)

proc floatNode(f: float64): JsonNode = newJFloat(f)

proc parseNumber(p: var Parser): JsonNode =
  var positive = true
  if p.peek == '-':
    positive = false
    inc p.i
  if p.peek notin {'0' .. '9'}: raise fail("invalid number")
  var significand = 0'u64
  if p.peek == '0':
    inc p.i
    if p.peek in {'0' .. '9'}: raise fail("invalid number")
  else:
    significand = uint64(ord(p.peek) - ord('0'))
    inc p.i
    while p.peek in {'0' .. '9'}:
      let digit = uint64(ord(p.peek) - ord('0'))
      if overflowU64(significand, digit): return floatNode(p.parseLongInteger(positive, significand))
      inc p.i
      significand = significand * 10 + digit
  case p.peek
  of '.': floatNode(p.parseDecimal(positive, significand, 0))
  of 'e', 'E': floatNode(p.parseExponent(positive, significand, 0))
  else:
    if positive:
      if significand <= uint64(high(int64)): newJInt(BiggestInt(significand))
      else: newJFloat(float64(significand)) # a `u64` past `i64::MAX` (std/json has no such node)
    else:
      let neg = cast[int64](0'u64 - significand)
      # A float on underflow, or for `-0`.
      if neg >= 0: newJFloat(-float64(significand)) else: newJInt(neg)

proc hex4(p: var Parser): int =
  if p.i + 4 > p.s.len: raise fail("EOF while parsing a string")
  for k in 0 ..< 4:
    let c = p.s[p.i + k]
    let v = (case c
             of '0' .. '9': ord(c) - ord('0')
             of 'a' .. 'f': ord(c) - ord('a') + 10
             of 'A' .. 'F': ord(c) - ord('A') + 10
             else: raise fail("invalid escape"))
    result = result * 16 + v
  p.i += 4

proc parseString(p: var Parser): string =
  inc p.i # opening quote
  while true:
    if p.i >= p.s.len: raise fail("EOF while parsing a string")
    let c = p.s[p.i]
    case c
    of '"':
      inc p.i
      return
    of '\\':
      inc p.i
      if p.i >= p.s.len: raise fail("EOF while parsing a string")
      let e = p.s[p.i]
      inc p.i
      case e
      of '"': result.add '"'
      of '\\': result.add '\\'
      of '/': result.add '/'
      of 'b': result.add '\b'
      of 'f': result.add '\f'
      of 'n': result.add '\n'
      of 'r': result.add '\r'
      of 't': result.add '\t'
      of 'u':
        var cp = p.hex4()
        if cp >= 0xD800 and cp <= 0xDBFF:
          if p.i + 1 < p.s.len and p.s[p.i] == '\\' and p.s[p.i + 1] == 'u':
            p.i += 2
            let lo = p.hex4()
            if lo < 0xDC00 or lo > 0xDFFF: raise fail("lone leading surrogate in hex escape")
            cp = 0x10000 + ((cp - 0xD800) shl 10) + (lo - 0xDC00)
          else: raise fail("unexpected end of hex escape")
        elif cp >= 0xDC00 and cp <= 0xDFFF: raise fail("lone leading surrogate in hex escape")
        result.add $Rune(cp)
      else: raise fail("invalid escape")
    else:
      if ord(c) < 0x20: raise fail("control character (\\u0000-\\u001F) found while parsing a string")
      result.add c
      inc p.i

proc parseValue(p: var Parser; depth: int): JsonNode =
  if depth > 128: raise fail("recursion limit exceeded")
  p.skipWs()
  case p.peek
  of '{':
    inc p.i
    result = newJObject()
    p.skipWs()
    if p.peek == '}':
      inc p.i
      return
    while true:
      p.skipWs()
      if p.peek != '"': raise fail("key must be a string")
      let k = p.parseString()
      p.skipWs()
      if p.peek != ':': raise fail("expected `:`")
      inc p.i
      result[k] = p.parseValue(depth + 1)
      p.skipWs()
      if p.peek == ',':
        inc p.i
      elif p.peek == '}':
        inc p.i
        return
      else: raise fail("expected `,` or `}`")
  of '[':
    inc p.i
    result = newJArray()
    p.skipWs()
    if p.peek == ']':
      inc p.i
      return
    while true:
      result.add p.parseValue(depth + 1)
      p.skipWs()
      if p.peek == ',':
        inc p.i
      elif p.peek == ']':
        inc p.i
        return
      else: raise fail("expected `,` or `]`")
  of '"': result = newJString(p.parseString())
  of 't':
    if p.s.continuesWith("true", p.i):
      p.i += 4
      return newJBool(true)
    raise fail("expected ident")
  of 'f':
    if p.s.continuesWith("false", p.i):
      p.i += 5
      return newJBool(false)
    raise fail("expected ident")
  of 'n':
    if p.s.continuesWith("null", p.i):
      p.i += 4
      return newJNull()
    raise fail("expected ident")
  of '-', '0' .. '9': result = p.parseNumber()
  else: raise fail("expected value")

proc parseSerdeJson*(s: string): JsonNode =
  ## Parses `s` like `serde_json::from_str::<Value>`; raises `SjsonError` on malformed input.
  var p = Parser(s: s)
  result = p.parseValue(0)
  p.skipWs()
  if p.i < p.s.len: raise fail("trailing characters")
