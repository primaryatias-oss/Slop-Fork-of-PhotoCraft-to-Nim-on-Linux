## Photoshop's `EngineData`: the PostScript-like text engine document stored in `TySh` (port of
## `photocraft_text::engine_data`).
##
## Syntax (from observation of real files and public notes): dictionaries `<< /Key value … >>`,
## arrays `[ … ]`, strings `( … )` holding UTF-16BE with a `FE FF` byte-order mark and `\`
## escapes, numbers (`12`, `-1.5`, `.5`), `true`/`false`, and `/Name` values.

import std/[options, strutils, unicode, math]

type
  EdKind* = enum ekDict, ekArray, ekString, ekInt, ekReal, ekBool, ekName
  EdValue* = object
    case kind*: EdKind
    of ekDict: items*: seq[(string, EdValue)]
    of ekArray: elems*: seq[EdValue]
    of ekString, ekName: s*: string
    of ekInt: i*: int64
    of ekReal: r*: float64
    of ekBool: b*: bool

  EngineDataError* = object of CatchableError

const MaxDepth = 64

# ---------------------------------------------------------------- constructors and access

proc edDict*(items: seq[(string, EdValue)] = @[]): EdValue = EdValue(kind: ekDict, items: items)
proc edArray*(elems: seq[EdValue] = @[]): EdValue = EdValue(kind: ekArray, elems: elems)
proc edString*(s: string): EdValue = EdValue(kind: ekString, s: s)
proc edName*(s: string): EdValue = EdValue(kind: ekName, s: s)
proc edInt*(i: int64): EdValue = EdValue(kind: ekInt, i: i)
proc edReal*(r: float64): EdValue = EdValue(kind: ekReal, r: r)
proc edBool*(b: bool): EdValue = EdValue(kind: ekBool, b: b)

proc `==`*(a, b: EdValue): bool =
  if a.kind != b.kind: return false
  case a.kind
  of ekDict:
    if a.items.len != b.items.len: return false
    for i in 0 ..< a.items.len:
      if a.items[i][0] != b.items[i][0] or a.items[i][1] != b.items[i][1]: return false
    true
  of ekArray:
    if a.elems.len != b.elems.len: return false
    for i in 0 ..< a.elems.len:
      if a.elems[i] != b.elems[i]: return false
    true
  of ekString, ekName: a.s == b.s
  of ekInt: a.i == b.i
  of ekReal: a.r == b.r
  of ekBool: a.b == b.b

proc getPtr*(v: EdValue; key: string): ptr EdValue =
  ## The entry `key` of a dictionary (nil when absent or not a dictionary).
  if v.kind != ekDict: return nil
  for i in 0 ..< v.items.len:
    if v.items[i][0] == key: return unsafeAddr v.items[i][1]
  nil

proc get*(v: EdValue; key: string): Option[EdValue] =
  let p = v.getPtr(key)
  if p == nil: none(EdValue) else: some(p[])

proc has*(v: EdValue; key: string): bool = v.getPtr(key) != nil

proc pathPtr*(v: EdValue; keys: openArray[string]): ptr EdValue =
  ## Follows a path of dictionary keys.
  var cur = unsafeAddr v
  for k in keys:
    cur = cur[].getPtr(k)
    if cur == nil: return nil
  cur

proc path*(v: EdValue; keys: openArray[string]): Option[EdValue] =
  let p = v.pathPtr(keys)
  if p == nil: none(EdValue) else: some(p[])

proc getMut*(v: var EdValue; key: string): ptr EdValue =
  if v.kind != ekDict: return nil
  for i in 0 ..< v.items.len:
    if v.items[i][0] == key: return addr v.items[i][1]
  nil

proc set*(v: var EdValue; key: string; x: EdValue) =
  ## Sets (or appends) a dictionary entry; no effect on other values.
  if v.kind != ekDict: return
  for i in 0 ..< v.items.len:
    if v.items[i][0] == key:
      v.items[i][1] = x
      return
  v.items.add (key, x)

proc remove*(v: var EdValue; key: string) =
  if v.kind != ekDict: return
  var i = 0
  while i < v.items.len:
    if v.items[i][0] == key: v.items.delete(i) else: inc i

proc asF64*(v: EdValue): Option[float64] =
  case v.kind
  of ekInt: some(float64(v.i))
  of ekReal: some(v.r)
  else: none(float64)

proc saturatingI64(r: float64): int64 =
  ## Rust's `f64 as i64`: truncates, saturates, NaN → 0.
  if r != r: 0'i64
  elif r >= 9.223372036854775807e18: high(int64)
  elif r <= -9.223372036854775808e18: low(int64)
  else: int64(r)

proc asI64*(v: EdValue): Option[int64] =
  case v.kind
  of ekInt: some(v.i)
  of ekReal: some(saturatingI64(v.r))
  else: none(int64)

proc asBool*(v: EdValue): Option[bool] =
  case v.kind
  of ekBool: some(v.b)
  of ekInt: some(v.i != 0)
  else: none(bool)

proc asStr*(v: EdValue): Option[string] =
  if v.kind in {ekString, ekName}: some(v.s) else: none(string)

proc isArray*(v: EdValue): bool = v.kind == ekArray
proc isDict*(v: EdValue): bool = v.kind == ekDict

# Option helpers for chained lookups (`e.get("A").get("B")`).
proc get*(v: Option[EdValue]; key: string): Option[EdValue] =
  if v.isSome: v.unsafeGet.get(key) else: none(EdValue)
proc path*(v: Option[EdValue]; keys: openArray[string]): Option[EdValue] =
  if v.isSome: v.unsafeGet.path(keys) else: none(EdValue)
proc asF64*(v: Option[EdValue]): Option[float64] =
  if v.isSome: v.unsafeGet.asF64 else: none(float64)
proc asI64*(v: Option[EdValue]): Option[int64] =
  if v.isSome: v.unsafeGet.asI64 else: none(int64)
proc asBool*(v: Option[EdValue]): Option[bool] =
  if v.isSome: v.unsafeGet.asBool else: none(bool)
proc asStr*(v: Option[EdValue]): Option[string] =
  if v.isSome: v.unsafeGet.asStr else: none(string)
proc asArray*(v: Option[EdValue]): Option[seq[EdValue]] =
  if v.isSome and v.unsafeGet.kind == ekArray: some(v.unsafeGet.elems) else: none(seq[EdValue])

# ---------------------------------------------------------------- parsing

type Parser = object
  b: ptr UncheckedArray[byte]
  n: int
  i: int

proc at(p: Parser; i: int): int =
  ## The byte at `i`, or -1 past the end.
  if i >= 0 and i < p.n: int(p.b[i]) else: -1

proc fail(p: Parser; m: string) {.noreturn.} =
  raise newException(EngineDataError, "EngineData: " & m & " at byte " & $p.i)

proc ws(p: var Parser) =
  while p.i < p.n and p.b[p.i] in {byte(' '), byte('\t'), byte('\r'), byte('\n'), 0'u8}: inc p.i

const Delims = {byte(' '), byte('\t'), byte('\r'), byte('\n'), byte('['), byte(']'), byte('<'), byte('>'),
                byte('('), byte('/')}

proc lossyUtf8(b: openArray[byte]): string =
  ## `String::from_utf8_lossy`: invalid sequences become U+FFFD.
  var s = newString(b.len)
  for i, c in b: s[i] = char(c)
  if validateUtf8(s) == -1: return s
  var i = 0
  while i < s.len:
    let n = validateUtf8(s.substr(i))
    if n == -1:
      result.add s.substr(i)
      break
    result.add s.substr(i, i + n - 1)
    # Skip the maximal invalid prefix the way Rust does: one byte, or a truncated sequence.
    var j = i + n
    let c = byte(s[j])
    var need = if c >= 0xF0 and c <= 0xF4: 3 elif c >= 0xE0 and c <= 0xEF: 2 elif c >= 0xC2 and c <= 0xDF: 1 else: 0
    var k = j + 1
    var lo = 0x80'u8
    var hi = 0xBF'u8
    if c == 0xE0: lo = 0xA0
    elif c == 0xED: hi = 0x9F
    elif c == 0xF0: lo = 0x90
    elif c == 0xF4: hi = 0x8F
    while need > 0 and k < s.len and byte(s[k]) >= lo and byte(s[k]) <= hi:
      inc k
      dec need
      lo = 0x80
      hi = 0xBF
    result.add "�"
    i = k

proc fromUtf16Lossy(u: openArray[uint16]): string =
  var i = 0
  while i < u.len:
    let c = int(u[i])
    if c >= 0xD800 and c <= 0xDBFF and i + 1 < u.len and int(u[i + 1]) >= 0xDC00 and int(u[i + 1]) <= 0xDFFF:
      result.add Rune(0x10000 + ((c - 0xD800) shl 10) + (int(u[i + 1]) - 0xDC00))
      i += 2
    elif c >= 0xD800 and c <= 0xDFFF:
      result.add "�"
      inc i
    else:
      result.add Rune(c)
      inc i

proc decodeString(raw: openArray[byte]): string =
  if raw.len >= 2 and raw[0] == 0xFE and raw[1] == 0xFF:
    var u = newSeqOfCap[uint16]((raw.len - 2) div 2)
    var i = 2
    while i + 1 < raw.len:
      u.add (uint16(raw[i]) shl 8) or uint16(raw[i + 1])
      i += 2
    result = fromUtf16Lossy(u)
  else:
    for c in raw: result.add Rune(int(c))   # Latin-1

proc parseRustF64*(t: string): Option[float64] =
  ## `str::parse::<f64>`: optional sign, then `inf`/`infinity`/`nan` (any case) or digits with an
  ## optional fraction and exponent (at least one mantissa digit).
  var i = 0
  var neg = false
  if i < t.len and t[i] in {'+', '-'}:
    neg = t[i] == '-'
    inc i
  let rest = t.substr(i).toLowerAscii
  if rest in ["inf", "infinity"]: return some(if neg: NegInf else: Inf)
  if rest == "nan": return some(NaN)
  var digits = 0
  var j = i
  while j < t.len and t[j] in Digits:
    inc j
    inc digits
  if j < t.len and t[j] == '.':
    inc j
    while j < t.len and t[j] in Digits:
      inc j
      inc digits
  if digits == 0: return none(float64)
  if j < t.len and t[j] in {'e', 'E'}:
    inc j
    if j < t.len and t[j] in {'+', '-'}: inc j
    var ed = 0
    while j < t.len and t[j] in Digits:
      inc j
      inc ed
    if ed == 0: return none(float64)
  if j != t.len: return none(float64)
  # Normalize for Nim's parser: a digit on both sides of the point.
  var s = t.substr(i)
  let dot = s.find('.')
  if dot >= 0:
    if dot == 0 or s[dot - 1] notin Digits: s.insert("0", dot)
    let d2 = s.find('.')
    if d2 + 1 >= s.len or s[d2 + 1] notin Digits: s.insert("0", d2 + 1)
  try:
    let v = parseFloat(s)
    some(if neg: -v else: v)
  except ValueError:
    none(float64)

proc parseRustI64*(t: string): Option[int64] =
  ## `str::parse::<i64>`: optional sign and decimal digits, no overflow.
  var i = 0
  var neg = false
  if i < t.len and t[i] in {'+', '-'}:
    neg = t[i] == '-'
    inc i
  if i >= t.len: return none(int64)
  var v: uint64 = 0
  let limit = if neg: 9223372036854775808'u64 else: 9223372036854775807'u64
  while i < t.len:
    if t[i] notin Digits: return none(int64)
    let d = uint64(ord(t[i]) - ord('0'))
    if v > (limit - d) div 10: return none(int64)
    v = v * 10 + d
    inc i
  if neg:
    if v == 9223372036854775808'u64: some(low(int64)) else: some(-int64(v))
  else: some(int64(v))

proc name(p: var Parser): string =
  inc p.i   # '/'
  let start = p.i
  while p.i < p.n and p.b[p.i] notin Delims: inc p.i
  lossyUtf8(toOpenArray(p.b, start, p.i - 1))

proc value(p: var Parser; depth: int): EdValue =
  if depth > MaxDepth: p.fail("nesting too deep")
  p.ws()
  let c = p.at(p.i)
  if c < 0: p.fail("unexpected end")
  if c == ord('<') and p.at(p.i + 1) == ord('<'):
    p.i += 2
    result = edDict()
    while true:
      p.ws()
      let d = p.at(p.i)
      if d == ord('>') and p.at(p.i + 1) == ord('>'):
        p.i += 2
        return
      elif d == ord('/'):
        let k = p.name()
        let v = p.value(depth + 1)
        result.items.add (k, v)
      elif d < 0: p.fail("unterminated dictionary")
      else: p.fail("expected key")
  elif c == ord('['):
    inc p.i
    result = edArray()
    while true:
      p.ws()
      let d = p.at(p.i)
      if d == ord(']'):
        inc p.i
        return
      elif d < 0: p.fail("unterminated array")
      else: result.elems.add p.value(depth + 1)
  elif c == ord('('):
    inc p.i
    var raw: seq[byte]
    while true:
      let d = p.at(p.i)
      if d < 0: p.fail("unterminated string")
      elif d == ord('\\'):
        let e = p.at(p.i + 1)
        if e >= 0: raw.add byte(e)
        p.i += 2
      elif d == ord(')'):
        inc p.i
        break
      else:
        raw.add byte(d)
        inc p.i
    return edString(decodeString(raw))
  elif c == ord('/'):
    return edName(p.name())
  else:
    let start = p.i
    while p.i < p.n and p.b[p.i] notin Delims: inc p.i
    var tok = newString(p.i - start)
    for k in 0 ..< tok.len: tok[k] = char(p.b[start + k])
    if validateUtf8(tok) != -1: tok = ""
    if tok == "true": return edBool(true)
    if tok == "false": return edBool(false)
    if tok == "":
      inc p.i
      p.fail("unexpected byte")
    if '.' in tok or 'e' in tok or 'E' in tok:
      let r = parseRustF64(tok)
      if r.isNone: p.fail("bad number")
      return edReal(r.get)
    let iv = parseRustI64(tok)
    if iv.isSome: return edInt(iv.get)
    let r = parseRustF64(tok)
    if r.isNone: p.fail("bad token")
    return edReal(r.get)

proc parseEngineData*(data: openArray[byte]): EdValue =
  ## Parses an EngineData document (the top-level value). Raises `EngineDataError`.
  if data.len == 0: raise newException(EngineDataError, "EngineData: unexpected end at byte 0")
  var p = Parser(b: cast[ptr UncheckedArray[byte]](unsafeAddr data[0]), n: data.len)
  p.value(0)

proc tryParseEngineData*(data: openArray[byte]): Option[EdValue] =
  try: some(parseEngineData(data))
  except EngineDataError: none(EdValue)

# ---------------------------------------------------------------- writing

proc formatReal*(r: float64): string =
  ## Photoshop style: `1.0`, `.5`, `-.25`.
  if r != r or r == Inf or r == NegInf: return "0.0"
  if r - trunc(r) == 0.0 and abs(r) < 1e15:
    return formatFloat(r, ffDecimal, 1)
  var s = formatFloat(r, ffDecimal, 5)
  while s.endsWith('0'): s.setLen(s.len - 1)
  if s == "0." or s == "-0.": return "0.0"
  if s.startsWith("0."): s = s.substr(1)
  elif s.startsWith("-0."): s = "-" & s.substr(2)
  s

proc indent(o: var seq[byte]; n: int) =
  for _ in 0 ..< n: o.add byte('\t')

proc addStr(o: var seq[byte]; s: string) =
  for c in s: o.add byte(c)

proc isContainer(v: EdValue): bool =
  if v.kind == ekDict: return true
  if v.kind == ekArray:
    for x in v.elems:
      if x.kind in {ekDict, ekArray}: return true
  false

proc utf16Units(s: string): seq[uint16] =
  for r in s.runes:
    let c = int(r)
    if c >= 0x10000:
      let v = c - 0x10000
      result.add uint16(0xD800 + (v shr 10))
      result.add uint16(0xDC00 + (v and 0x3FF))
    else: result.add uint16(c)

proc writeValue(v: EdValue; depth: int; o: var seq[byte]) =
  case v.kind
  of ekDict:
    o.addStr "<<\n"
    for (k, x) in v.items:
      o.indent(depth + 1)
      o.add byte('/')
      o.addStr k
      if isContainer(x):
        o.add byte('\n')
        o.indent(depth + 1)
      else:
        o.add byte(' ')
      writeValue(x, depth + 1, o)
      o.add byte('\n')
    o.indent(depth)
    o.addStr ">>"
  of ekArray:
    if isContainer(v):
      o.addStr "[\n"
      for x in v.elems:
        o.indent(depth)
        writeValue(x, depth, o)
        o.add byte('\n')
      o.indent(depth)
      o.add byte(']')
    else:
      o.add byte('[')
      for x in v.elems:
        o.add byte(' ')
        writeValue(x, depth, o)
      o.addStr " ]"
  of ekString:
    o.add byte('(')
    var raw = @[0xFE'u8, 0xFF]
    for u in utf16Units(v.s):
      raw.add byte(u shr 8)
      raw.add byte(u and 0xFF)
    for c in raw:
      if c in {byte('('), byte(')'), byte('\\')}: o.add byte('\\')
      o.add c
    o.add byte(')')
  of ekInt: o.addStr $v.i
  of ekReal: o.addStr formatReal(v.r)
  of ekBool: o.addStr(if v.b: "true" else: "false")
  of ekName:
    o.add byte('/')
    o.addStr v.s

proc writeEngineData*(v: EdValue): seq[byte] =
  ## Serializes in Photoshop's layout (tab-indented, one key per line).
  result = @[byte('\n'), byte('\n')]
  writeValue(v, 0, result)
  result.add byte('\n')

proc writeBare*(items: openArray[(string, EdValue)]): seq[byte] =
  ## Serializes `/key value` pairs without an enclosing dictionary: the layout of the document's
  ## `Txt2` block, which `parseTxt2` wraps in `<< >>` to parse.
  result = @[byte('\n'), byte('\n')]
  for (k, v) in items:
    result.add byte('/')
    result.addStr k
    if isContainer(v):
      result.add byte('\n')
      result.indent(1)
    else:
      result.add byte(' ')
    writeValue(v, 1, result)
    result.add byte('\n')
