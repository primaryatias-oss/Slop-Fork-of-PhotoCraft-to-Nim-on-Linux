## Small text helpers shared across layers (Rust standard-library string semantics).

import std/unicode

proc lossyUtf8*(b: openArray[byte]): string =
  ## `String::from_utf8_lossy`: each maximal invalid sequence becomes U+FFFD.
  var i = 0
  while i < b.len:
    let c = int(b[i])
    if c < 0x80:
      result.add char(c); inc i; continue
    var (n, cp, lo, hi) = (0, 0, 0x80, 0xBF)
    if c >= 0xC2 and c <= 0xDF: (n, cp) = (1, c and 0x1F)
    elif c >= 0xE0 and c <= 0xEF:
      (n, cp) = (2, c and 0x0F)
      if c == 0xE0: lo = 0xA0
      elif c == 0xED: hi = 0x9F
    elif c >= 0xF0 and c <= 0xF4:
      (n, cp) = (3, c and 0x07)
      if c == 0xF0: lo = 0x90
      elif c == 0xF4: hi = 0x8F
    else:
      result.add "\xEF\xBF\xBD"; inc i; continue
    var j = 1
    while j <= n and i + j < b.len:
      let x = int(b[i + j])
      if x < (if j == 1: lo else: 0x80) or x > (if j == 1: hi else: 0xBF): break
      cp = (cp shl 6) or (x and 0x3F)
      inc j
    if j == n + 1:
      for k in 0 .. n: result.add char(b[i + k])
    else:
      result.add "\xEF\xBF\xBD"
    i += j

proc trimRust*(s: string): string =
  ## `str::trim`: strips leading and trailing Unicode white space.
  var a = 0
  var b = s.len
  template ws(r: Rune): bool = isWhiteSpace(r)
  while a < b:
    var i = a
    var r: Rune
    fastRuneAt(s, i, r, true)
    if not ws(r): break
    a = i
  while b > a:
    var j = b - 1
    while j > a and (ord(s[j]) and 0xC0) == 0x80: dec j
    var k = j
    var r: Rune
    fastRuneAt(s, k, r, true)
    if not ws(r): break
    b = j
  s[a ..< b]
