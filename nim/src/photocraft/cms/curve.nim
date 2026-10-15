## One-dimensional tone curves (ICC `curv` and `para` tag types) and their inverses.
##
## Gamma and parametric curves are evaluated analytically, including outside `[0, 1]` (negative
## inputs mirror, values above 1 extend the formula), so unbounded float transforms keep HDR
## values. Sampled tables clamp their input to `[0, 1]`.

import std/math
import ../rmath

type
  CurveKind* = enum ckIdentity, ckGamma, ckParametric, ckTable
  Curve* = object
    case kind*: CurveKind
    of ckIdentity: discard
    of ckGamma: gamma*: float64           ## `y = x^g`
    of ckParametric:
      pkind*: int                          ## ICC parametric function type 0–4
      p*: array[7, float64]                ## `[g, a, b, c, d, e, f]`
    of ckTable: table*: seq[float32]       ## evenly spaced samples over [0, 1]

proc identityCurve*(): Curve = Curve(kind: ckIdentity)
proc gammaCurve*(g: float64): Curve = Curve(kind: ckGamma, gamma: g)
proc paraCurve*(kind: int; p: array[7, float64]): Curve = Curve(kind: ckParametric, pkind: kind, p: p)
proc tableCurve*(t: seq[float32]): Curve = Curve(kind: ckTable, table: t)

proc `==`*(a, b: Curve): bool =
  if a.kind != b.kind: return false
  case a.kind
  of ckIdentity: true
  of ckGamma: a.gamma == b.gamma
  of ckParametric: a.pkind == b.pkind and a.p == b.p
  of ckTable: a.table == b.table

proc srgbTrc*(): Curve =
  ## The sRGB transfer function (IEC 61966-2.1) as ICC parametric type 3.
  paraCurve(3, [2.4, 1.0 / 1.055, 0.055 / 1.055, 1.0 / 12.92, 0.04045, 0.0, 0.0])

proc paramCount*(kind: int): int =
  ## Number of parameters stored for a parametric function type.
  case kind
  of 0: 1
  of 1: 3
  of 2: 4
  of 3: 5
  else: 7

template mirror(x: float64; body: untyped): float64 =
  ## Applies `body` (using `v`) to |x| and restores the sign.
  (if x < 0.0: (let v {.inject.} = -x; -(body)) else: (let v {.inject.} = x; body))

proc pw(base, g: float64): float64 {.inline.} = (if base <= 0.0: 0.0 else: pow(base, g))

proc para(kind: int; p: array[7, float64]; x: float64): float64 =
  let (g, a, b, c, d, e, f) = (p[0], p[1], p[2], p[3], p[4], p[5], p[6])
  case kind
  of 0: mirror(x, pow(v, g))
  of 1:
    if a != 0.0 and x >= -b / a: pw(a * x + b, g) else: 0.0
  of 2:
    if a != 0.0 and x >= -b / a: pw(a * x + b, g) + c else: c
  of 3:
    if x < 0.0 and b == 0.0 and d == 0.0: -para(kind, p, -x)
    elif x >= d: pw(a * x + b, g)
    else: c * x
  else:
    if x >= d: pw(a * x + b, g) + e else: c * x + f

proc paraInverse(kind: int; p: array[7, float64]; y: float64; ok: var bool): float64 =
  let (g, a, b, c, d, e, f) = (p[0], p[1], p[2], p[3], p[4], p[5], p[6])
  ok = true
  if abs(g) < 1e-12:
    ok = false
    return 0.0
  proc root(v: float64): float64 = (if v <= 0.0: 0.0 else: pow(v, 1.0 / g))
  case kind
  of 0: mirror(y, pow(v, 1.0 / g))
  of 1:
    if a == 0.0:
      ok = false
      return 0.0
    if y <= 0.0: -b / a else: (root(y) - b) / a
  of 2:
    if a == 0.0:
      ok = false
      return 0.0
    if y <= c: -b / a else: (root(y - c) - b) / a
  of 3:
    if a == 0.0:
      ok = false
      return 0.0
    if y < 0.0 and b == 0.0 and d == 0.0:
      return -paraInverse(kind, p, -y, ok)
    let yd = pw(a * d + b, g)
    if y >= yd: (root(y) - b) / a
    elif c != 0.0: y / c
    else: 0.0
  else:
    if a == 0.0:
      ok = false
      return 0.0
    let yd = pw(a * d + b, g) + e
    if y >= yd: (root(y - e) - b) / a
    elif c != 0.0: (y - f) / c
    else: 0.0

proc numericInverse(kind: int; p: array[7, float64]; y: float64): float64 =
  ## Bisection inverse of a monotonic parametric function on `[0, 1]`.
  let f0 = para(kind, p, 0.0)
  let f1 = para(kind, p, 1.0)
  let increasing = f1 >= f0
  var lo = 0.0
  var hi = 1.0
  for _ in 0 ..< 60:
    let mid = 0.5 * (lo + hi)
    if (para(kind, p, mid) < y) == increasing: lo = mid else: hi = mid
  0.5 * (lo + hi)

proc tableEval(t: seq[float32]; x: float32): float32 {.inline.} =
  let n = t.len
  case n
  of 0: x
  of 1: t[0]
  else:
    let pos = clamp(x, 0.0'f32, 1.0'f32) * float32(n - 1)
    let i = min(asIdx(pos), n - 2)
    let f = pos - float32(i)
    t[i] + (t[i + 1] - t[i]) * f

proc edgeIndex(t: seq[float32]; start: bool): float32 =
  let n = t.len
  if start:
    let v = t[0]
    var i = n - 1
    for k in 0 ..< n:
      if abs(t[k] - v) > 1e-7:
        i = k - 1
        break
    float32(i) / float32(n - 1)
  else:
    let v = t[n - 1]
    var i = 0
    for k in countdown(n - 1, 0):
      if abs(t[k] - v) > 1e-7:
        i = k + 1
        break
    float32(i) / float32(n - 1)

proc tableInverse(t: seq[float32]; y: float32): float32 =
  ## Exact inverse of a piecewise-linear table (binary search), increasing or decreasing.
  let n = t.len
  if n < 2: return y
  let increasing = t[n - 1] >= t[0]
  let (loV, hiV) = if increasing: (t[0], t[n - 1]) else: (t[n - 1], t[0])
  if y <= loV: return (if increasing: edgeIndex(t, true) else: edgeIndex(t, false))
  if y >= hiV: return (if increasing: edgeIndex(t, false) else: edgeIndex(t, true))
  var lo = 0
  var hi = n - 1
  while hi - lo > 1:
    let mid = (lo + hi) div 2
    if (t[mid] < y) == increasing: lo = mid else: hi = mid
  let a = t[lo]
  let b = t[hi]
  let f = if abs(b - a) > 1e-12: (y - a) / (b - a) else: 0.5'f32
  (float32(lo) + f) / float32(n - 1)

proc isIdentity*(c: Curve): bool =
  case c.kind
  of ckIdentity: true
  of ckGamma: abs(c.gamma - 1.0) < 1e-9
  of ckParametric: c.pkind == 0 and abs(c.p[0] - 1.0) < 1e-9
  of ckTable:
    let n = c.table.len
    if n < 2: return true
    for i, v in c.table:
      if abs(float64(v) - float64(i) / float64(n - 1)) >= 1.0 / 65535.0: return false
    true

proc eval64*(c: Curve; x: float64): float64 =
  ## Evaluates the curve at `x` (f64 reference path).
  case c.kind
  of ckIdentity: x
  of ckGamma: mirror(x, pow(v, c.gamma))
  of ckParametric: para(c.pkind, c.p, x)
  of ckTable: float64(tableEval(c.table, float32(x)))

proc eval*(c: Curve; x: float32): float32 {.inline.} =
  case c.kind
  of ckIdentity: x
  of ckTable: tableEval(c.table, x)
  else: float32(c.eval64(float64(x)))

proc evalInverse64*(c: Curve; y: float64): float64 =
  case c.kind
  of ckIdentity: y
  of ckGamma:
    if abs(c.gamma) < 1e-12: y else: mirror(y, pow(v, 1.0 / c.gamma))
  of ckParametric:
    var ok: bool
    let r = paraInverse(c.pkind, c.p, y, ok)
    if ok: r else: numericInverse(c.pkind, c.p, y)
  of ckTable: float64(tableInverse(c.table, float32(y)))

proc evalInverse*(c: Curve; y: float32): float32 {.inline.} =
  case c.kind
  of ckIdentity: y
  of ckTable: tableInverse(c.table, y)
  else: float32(c.evalInverse64(float64(y)))

proc sample*(c: Curve; n: int): seq[float32] =
  ## Samples `n` evenly spaced points over `[0, 1]`.
  result = newSeq[float32](n)
  for i in 0 ..< n: result[i] = float32(c.eval64(float64(i) / float64(n - 1)))

proc isMonotonic*(c: Curve): bool =
  let s = c.sample(1024)
  var inc = true
  var dec = true
  for i in 1 ..< s.len:
    if not (s[i] >= s[i - 1] - 1e-6): inc = false
    if not (s[i] <= s[i - 1] + 1e-6): dec = false
  inc or dec
