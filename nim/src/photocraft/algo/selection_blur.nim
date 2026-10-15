## Fast mask blurs behind Select › Modify › Smooth and Feather (port of `selection_blur.rs`).
##
## Both are separable and give the same numbers as the direct definitions:
##
## * **Smooth** is a box average with edge-replicated borders: a running sum per line, `O(1)` per
##   pixel whatever the radius.
## * **Feather** is a Gaussian (`σ = radius`, as Photoshop's, kernel truncated at `⌈3σ⌉` and
##   normalized, zero outside the canvas). Small kernels are convolved directly. Wide ones are
##   written as a short cosine series over the kernel window, `k(j) ≈ a₀ + Σ aₖ cos(ωₖ j)`, and every
##   term is a sliding windowed sum `Σ m[x+j]·e^{iωₖj}` updated in `O(1)` per pixel, so the cost
##   doesn't grow with the radius. The series is fitted per call by least squares and its error is
##   checked (≤ 0.3/255 per pass); a fit that misses falls back to the direct convolution.
##
## Work is confined to the mask's non-zero bounding box grown by the kernel radius (pixels farther
## away stay exactly 0). The vertical pass walks strips of columns row by row. Windows lying
## entirely in a constant run (inside or outside the selection) are written without arithmetic.

import std/[math, options]
import ../rmath, ./util

const
  Terms = 5           ## cosine terms in the wide-kernel series
  DirectMaxR = 12     ## kernels up to this radius are convolved directly
  FitTol = 0.3 / 255.0 ## largest error (mask units, per pass) a cosine fit may have
  MaxSigma = 100_000'f32 ## σ cap: keeps the kernel arithmetic finite for absurd radii
  MaxBoxR = 1 shl 24  ## box radius cap for the same reason
  Strip = 64          ## columns per strip in the vertical pass

type
  Win = object
    ## A window of the mask: columns `x0..x1`, rows `y0..y1`.
    x0, x1, y0, y1: int

  Rows = object
    ## The horizontal pass's output as the vertical pass reads it: `bh` rows of `gw` samples
    ## placed at window rows `off..off + bh` of a window `gh` rows tall (other rows are zero).
    tmp: seq[float32]
    gw, bh, off, gh: int

  CosKernel = object
    ## Sliding cosine-series form of a symmetric kernel over `-r..=r`.
    r: int
    a0: float64
    a: array[Terms, float64]
    rot: array[Terms, (float64, float64)]   ## `e^{-iω}`: shifts the window by one sample
    outW: array[Terms, (float64, float64)]  ## `e^{-iωr}` (leaving sample)
    inW: array[Terms, (float64, float64)]   ## `e^{iωr}` (entering sample)
    full: array[Terms, float64]             ## `Σ_{|j|≤r} cos(ωj)`: a window full of ones
    omega: array[Terms, float64]

  KernelKind = enum kDirect, kCos
  Kernel = object
    case kind: KernelKind
    of kDirect: taps: seq[float32] ## taps for `-r..=r`
    of kCos: ck: CosKernel

proc w(b: Win): int {.inline.} = b.x1 - b.x0
proc h(b: Win): int {.inline.} = b.y1 - b.y0
proc satSub(a, b: int): int {.inline.} = max(a - b, 0)

proc get(r: Rows; yy, c0, sw: int): int =
  ## Start of columns `c0..c0 + sw` of window row `yy` in `tmp`, or -1 for a zero row.
  let rr = yy - r.off
  if yy < r.off or rr >= r.bh: return -1
  let s = rr * r.gw + c0
  if s < 0 or s + sw > r.tmp.len: -1 else: s

proc nonzeroBounds(m: openArray[float32]; w, h: int): Option[Win] =
  ## Bounding box of the non-zero samples (NaN counts as non-zero), or none when all are 0.
  var b = none(Win)
  for y in 0 ..< h:
    if (y + 1) * w > m.len: break
    var first = -1
    var last = -1
    for x in 0 ..< w:
      if m[y * w + x] != 0:
        if first < 0: first = x
        last = x
    if first < 0: continue
    let (a, z) = (first, last + 1)
    if b.isNone: b = some(Win(x0: a, x1: z, y0: y, y1: y + 1))
    else:
      let o = b.get
      b = some(Win(x0: min(o.x0, a), x1: max(o.x1, z), y0: o.y0, y1: y + 1))
  b

proc grow(b: Win; g, w, h: int): Win =
  ## `b` grown by `g` on every side, clamped to `w × h`.
  let gs = (if g > high(int) div 4: high(int) div 4 else: g)
  Win(x0: satSub(b.x0, gs), x1: min(b.x1 + gs, w), y0: satSub(b.y0, gs), y1: min(b.y1 + gs, h))

proc strips(gw: int; f: proc(c0, sw: int): seq[float32]): seq[seq[float32]] =
  ## Runs the vertical pass `f(c0, sw)` on strips of `Strip` columns.
  let k = (gw + Strip - 1) div Strip
  for s in 0 ..< k:
    let c0 = s * Strip
    result.add f(c0, min(Strip, gw - c0))

proc assemble(strips: seq[seq[float32]]; win: Win; w, h: int; outside: float32;
              f: proc(v: float32): float32): seq[float32] =
  ## Writes the strips into a fresh `w × h` mask through `f`; outside the window it is `outside`.
  result = newSeqWithValue(w * h, outside)
  for y in win.y0 ..< min(win.y1, h):
    let yy = y - win.y0
    for s, strip in strips:
      let c0 = s * Strip
      let sw = min(Strip, satSub(win.w, c0))
      if (yy + 1) * sw > strip.len or win.x0 + c0 + sw > w: continue
      for t in 0 ..< sw:
        result[y * w + win.x0 + c0 + t] = f(strip[yy * sw + t])

proc addRow(acc: var seq[float64]; rows: Rows; yy, c0: int; k: float64) =
  ## Adds `k ×` window row `yy` (columns `c0..c0 + acc.len`) into `acc`.
  if k == 0: return
  let s = rows.get(yy, c0, acc.len)
  if s < 0: return
  for i in 0 ..< acc.len: acc[i] += k * float64(rows.tmp[s + i])

# ---- Smooth: box average, edges replicated -------------------------------------------------

proc boxLine(src: openArray[float32]; dst: var openArray[float32]; r: int) =
  ## Running-sum box average of one line (radius `r`, edges replicated) into `dst`.
  let n = min(src.len, dst.len)
  if n == 0: return
  let (first, last) = (src[0], src[n - 1])
  if r == 0:
    for i in 0 ..< n: dst[i] = src[i]
    return
  template at(i: int): float64 = float64(src[clamp(i, 0, n - 1)])
  # Window at x = 0: r copies of the first sample, then src[0..=r], then copies of the last.
  let inside = min(r, n - 1)
  var s = -0.0
  for i in 0 .. inside: s += float64(src[i])
  var acc = float64(r) * float64(first) + s + float64(r - inside) * float64(last)
  let norm = 1.0 / float64(2 * r + 1)
  for x in 0 ..< n:
    dst[x] = float32(acc * norm)
    acc += at(x + r + 1) - at(x - r)

proc boxStrip(rows: Rows; c0, sw, r: int): seq[float32] =
  ## The vertical box pass over columns `c0..c0 + sw` (edges replicated).
  let gh = rows.gh
  result = newSeq[float32](gh * sw)
  if gh == 0 or sw == 0: return
  var acc = newSeq[float64](sw)
  # Window at row 0: r copies of row 0, rows 0..=r, then copies of the last row.
  let inside = min(r, gh - 1)
  addRow(acc, rows, 0, c0, float64(r))
  for j in 0 .. inside: addRow(acc, rows, j, c0, 1.0)
  addRow(acc, rows, gh - 1, c0, float64(r - inside))
  let norm = 1.0 / float64(2 * r + 1)
  for yy in 0 ..< gh:
    for t in 0 ..< sw: result[yy * sw + t] = float32(acc[t] * norm)
    addRow(acc, rows, min(yy + r + 1, gh - 1), c0, 1.0)
    addRow(acc, rows, satSub(yy, r), c0, -1.0)

proc curve(v: float32): float32 = (if v >= 0.5'f32: 1'f32 else: 0'f32)

proc smooth*(m: openArray[float32]; w, h: int; r0: float32): seq[float32] =
  ## Select › Modify › Smooth: box average of radius `round(r)` (edges replicated), then a hard
  ## threshold at ½, as in Photoshop: straight edges stay exactly where they were, corners round
  ## off and specks drop.
  let n = (if w > 0 and h > high(int) div w: high(int) else: w * h)
  if m.len != n or n == 0:
    result = newSeq[float32](m.len)
    for i, v in m: result[i] = curve(v)
    return
  let r =
    if r0.classify notin {fcNan, fcInf, fcNegInf}: min(asIdx(rmax(r0, 0'f32).round), MaxBoxR)
    elif r0 > 0: MaxBoxR
    else: 0
  let bo = nonzeroBounds(m, w, h)
  if bo.isNone: return newSeqWithValue(n, curve(0))
  let b = bo.get
  # Grown one past the radius, so a window edge inside the canvas sits on zeros and edge
  # replication there equals the zeros beyond it.
  let win = grow(b, r + 1, w, h)
  let (gw, gh) = (win.w, win.h)
  # Rows: only the bounding box's rows are non-zero.
  var tmp = newSeq[float32](b.h * gw)
  for y in 0 ..< b.h:
    let o = (b.y0 + y) * w + win.x0
    if o + gw <= m.len:
      boxLine(m.toOpenArray(o, o + gw - 1), tmp.toOpenArray(y * gw, y * gw + gw - 1), r)
  # Columns: a running sum of rows per strip of columns.
  let rows = Rows(tmp: move(tmp), gw: gw, bh: b.h, off: b.y0 - win.y0, gh: gh)
  let res = strips(gw, proc(c0, sw: int): seq[float32] = boxStrip(rows, c0, sw, r))
  assemble(res, win, w, h, curve(0), curve)

# ---- Feather: truncated Gaussian, zero outside -----------------------------------------------

proc solve(a0: seq[seq[float64]]; b0: seq[float64]): Option[seq[float64]] =
  ## Solves the small dense system `a·x = b` (partial pivoting); none when singular.
  var a = a0
  var b = b0
  let n = b.len
  for c in 0 ..< n:
    # `max_by` keeps the last of equal maxima.
    var p = c
    for i in c + 1 ..< n:
      if totalCmp(abs(a[i][c]), abs(a[p][c])) >= 0: p = i
    if abs(a[p][c]) < 1e-12: return none(seq[float64])
    swap(a[c], a[p])
    swap(b[c], b[p])
    for i in c + 1 ..< n:
      let f = a[i][c] / a[c][c]
      for j in c ..< n: a[i][j] -= f * a[c][j]
      b[i] -= f * b[c]
  var x = newSeq[float64](n)
  for i in countdown(n - 1, 0):
    var s = -0.0
    for j in i + 1 ..< n: s += a[i][j] * x[j]
    x[i] = (b[i] - s) / a[i][i]
  for v in x:
    if v.classify in {fcNan, fcInf, fcNegInf}: return none(seq[float64])
  some(x)

proc newKernel(sigma: float64; r, n: int): Kernel =
  ## The feather kernel (σ, truncation radius `r`) as applied along lines of length `n`.
  # Normalized over the full truncated window, as the direct definition does.
  proc g(j: float64): float64 = exp(-(j * j) / (2.0 * sigma * sigma))
  var s = -0.0
  for j in 1 .. r: s += g(float64(j))
  let total = 1.0 + 2.0 * s
  # Only offsets inside the line matter.
  let jr = min(r, satSub(n, 1))
  if jr == 0:
    # One-sample line: the centre tap alone.
    return Kernel(kind: kDirect, taps: @[float32(1.0 / total)])
  var taps = newSeq[float64](jr + 1)
  for j in 0 .. jr: taps[j] = g(float64(j)) / total
  proc direct(): Kernel =
    var t = newSeq[float32](2 * jr + 1)
    for i in 0 .. 2 * jr: t[i] = float32(taps[abs(i - jr)])
    Kernel(kind: kDirect, taps: t)
  if jr <= DirectMaxR: return direct()
  # Least squares over j = -jr..=jr (symmetric: weight 2 for j > 0).
  let l = float64(jr + 1)
  var omega: array[Terms, float64]
  for k in 0 ..< Terms: omega[k] = PI * float64(k + 1) / l
  proc basis(q, j: int): float64 = (if q == 0: 1.0 else: cos(omega[q - 1] * float64(j)))
  proc wt(j: int): float64 = (if j == 0: 1.0 else: 2.0)
  var a = newSeq[seq[float64]](Terms + 1)
  for p in 0 .. Terms: a[p] = newSeq[float64](Terms + 1)
  var b = newSeq[float64](Terms + 1)
  for j, t in taps:
    var phi: array[Terms + 1, float64]
    for q in 0 .. Terms: phi[q] = basis(q, j)
    for p in 0 .. Terms:
      b[p] += wt(j) * phi[p] * t
      for q in 0 .. Terms: a[p][q] += wt(j) * phi[p] * phi[q]
  let co = solve(a, b)
  if co.isNone: return direct()
  let c = co.get
  # Error bound per pass: half the L1 distance between the series and the kernel.
  var es = -0.0
  for j, t in taps:
    var sq = -0.0
    for q in 0 .. Terms: sq += c[q] * basis(q, j)
    es += wt(j) * abs(sq - t)
  let err = es / 2.0
  if err.classify in {fcNan, fcInf, fcNegInf} or err > FitTol: return direct()
  let jf = float64(jr)
  var ck = CosKernel(r: jr, a0: c[0], omega: omega)
  for k in 0 ..< Terms:
    var fs = -0.0
    for j in 1 .. jr: fs += cos(omega[k] * float64(j))
    ck.full[k] = 1.0 + 2.0 * fs
    ck.a[k] = c[k + 1]
    ck.rot[k] = (cos(omega[k]), -sin(omega[k]))
    ck.outW[k] = (cos(omega[k] * jf), -sin(omega[k] * jf))
    ck.inW[k] = (cos(omega[k] * jf), sin(omega[k] * jf))
  Kernel(kind: kCos, ck: ck)

proc line(k: CosKernel; src: openArray[float32]; dst: var openArray[float32]; runs: var seq[int]) =
  let n = src.len
  let r = k.r
  # runs[i]: end (exclusive) of the constant run containing i.
  runs.setLen(0)
  for i in 0 ..< n: runs.add n
  for i in countdown(n - 2, 0):
    runs[i] = (if src[i] == src[i + 1]: runs[i + 1] else: i + 1)
  template at(i: int): float64 = (if i < n: float64(src[i]) else: 0.0)
  # Window sums for the last position handled (valid when `have`).
  var have = false
  var sb = 0.0
  var s: array[Terms, (float64, float64)]
  for x in 0 ..< n:
    let lo = satSub(x, r)
    let hi = min(x + r + 1, n)
    let c = src[lo]
    if runs[lo] >= hi and (c == 0 or (x >= r and x + r < n)):
      # The whole window is one value (zeros may run off the line; ones may not).
      dst[x] = c
      let cf = float64(c)
      sb = cf * float64(2 * r + 1)
      for t in 0 ..< Terms: s[t] = (cf * k.full[t], 0.0)
      have = true
      continue
    if have:
      # Slide from x - 1: drop src[x - 1 - r], take src[x + r].
      let gone = (if x > r: at(x - 1 - r) else: 0.0)
      let come = at(x + r)
      sb += come - gone
      for t in 0 ..< Terms:
        let (re, im) = (s[t][0] - gone * k.outW[t][0], s[t][1] - gone * k.outW[t][1])
        let (cr, ci) = k.rot[t]
        s[t] = (re * cr - im * ci + come * k.inW[t][0], re * ci + im * cr + come * k.inW[t][1])
    else:
      # Direct sums over the in-line part of the window.
      sb = 0.0
      for t in 0 ..< Terms: s[t] = (0.0, 0.0)
      for i in lo ..< hi:
        let v = float64(src[i])
        if v == 0: continue
        let j = float64(i) - float64(x)
        sb += v
        for t in 0 ..< Terms:
          s[t][0] += v * cos(k.omega[t] * j)
          s[t][1] += v * sin(k.omega[t] * j)
      have = true
    var ss = -0.0
    for t in 0 ..< Terms: ss += k.a[t] * s[t][0]
    dst[x] = float32(k.a0 * sb + ss)

proc line(k: Kernel; src: openArray[float32]; dst: var openArray[float32]; runs: var seq[int]) =
  ## Convolves one line (zero outside it) into `dst`.
  let n = min(src.len, dst.len)
  case k.kind
  of kDirect:
    let r = k.taps.len div 2
    for i in 0 ..< n: dst[i] = 0
    for i, kk in k.taps:
      # Output x reads src[x + i - r].
      let (d0, s0) = (if i >= r: (0, i - r) else: (r - i, 0))
      if d0 >= n or s0 >= n: continue
      let len = min(n - d0, n - s0)
      for t in 0 ..< len: dst[d0 + t] += kk * src[s0 + t]
  of kCos: k.ck.line(src.toOpenArray(0, n - 1), dst.toOpenArray(0, n - 1), runs)

proc strip(k: CosKernel; rows: Rows; c0, sw: int): seq[float32] =
  ## `line` down columns `c0..c0 + sw`, all columns of a row at once.
  let gh = rows.gh
  let r = k.r
  result = newSeq[float32](gh * sw)
  if sw == 0: return
  # Rows whose strip segment is one value, and the runs of equal such rows.
  var flag = newSeq[Option[float32]](gh)
  for yy in 0 ..< gh:
    let st = rows.get(yy, c0, sw)
    if st < 0: flag[yy] = some(0'f32)
    else:
      let c = rows.tmp[st]
      var same = true
      for t in 0 ..< sw:
        if rows.tmp[st + t] != c:
          same = false
          break
      flag[yy] = (if same: some(c) else: none(float32))
  var runs = newSeqWithValue(gh, gh)
  for i in countdown(gh - 2, 0):
    runs[i] = (if flag[i].isSome and flag[i + 1].isSome and flag[i].get == flag[i + 1].get: runs[i + 1] else: i + 1)
  var sb = newSeq[float64](sw)
  var re, im: array[Terms, seq[float64]]
  for t in 0 ..< Terms:
    re[t] = newSeq[float64](sw)
    im[t] = newSeq[float64](sw)
  var acc = newSeq[float64](sw)
  var have = false
  template rowAt(i, s: int): float64 = (if s < 0: 0.0 else: float64(rows.tmp[s + i]))
  for yy in 0 ..< gh:
    let lo = satSub(yy, r)
    let hi = min(yy + r + 1, gh)
    # The whole window is one value (zero rows may run off the window; others may not).
    if flag[lo].isSome and runs[lo] >= hi and (flag[lo].get == 0 or (yy >= r and yy + r < gh)):
      let c = flag[lo].get
      for t in 0 ..< sw: result[yy * sw + t] = c
      let cf = float64(c)
      for v in sb.mitems: v = cf * float64(2 * r + 1)
      for t in 0 ..< Terms:
        for v in re[t].mitems: v = cf * k.full[t]
        for v in im[t].mitems: v = 0.0
      have = true
      continue
    if have:
      # Slide from yy - 1: drop row yy - 1 - r, take row yy + r.
      let gone = (if yy > r: rows.get(yy - 1 - r, c0, sw) else: -1)
      let come = rows.get(yy + r, c0, sw)
      for i in 0 ..< sw: sb[i] += rowAt(i, come) - rowAt(i, gone)
      for t in 0 ..< Terms:
        let (cr, ci) = k.rot[t]
        let (o0, o1) = k.outW[t]
        let (i0, i1) = k.inW[t]
        for i in 0 ..< sw:
          let (g, c) = (rowAt(i, gone), rowAt(i, come))
          let (a, b) = (re[t][i] - g * o0, im[t][i] - g * o1)
          re[t][i] = a * cr - b * ci + c * i0
          im[t][i] = a * ci + b * cr + c * i1
    else:
      # Direct sums over the in-window rows.
      for v in sb.mitems: v = 0.0
      for t in 0 ..< Terms:
        for v in re[t].mitems: v = 0.0
        for v in im[t].mitems: v = 0.0
      for i in lo ..< hi:
        let st = rows.get(i, c0, sw)
        if st < 0: continue
        let j = float64(i) - float64(yy)
        addRow(sb, rows, i, c0, 1.0)
        for t in 0 ..< Terms:
          let (cs, sn) = (cos(k.omega[t] * j), sin(k.omega[t] * j))
          for q in 0 ..< sw:
            let v = float64(rows.tmp[st + q])
            re[t][q] += v * cs
            im[t][q] += v * sn
      have = true
    for i in 0 ..< sw: acc[i] = k.a0 * sb[i]
    for t in 0 ..< Terms:
      for i in 0 ..< sw: acc[i] += k.a[t] * re[t][i]
    for i in 0 ..< sw: result[yy * sw + i] = float32(acc[i])

proc strip(k: Kernel; rows: Rows; c0, sw: int): seq[float32] =
  ## The vertical pass over columns `c0..c0 + sw` (zero outside the window).
  case k.kind
  of kDirect:
    let r = k.taps.len div 2
    result = newSeq[float32](rows.gh * sw)
    if sw == 0: return
    for yy in 0 ..< rows.gh:
      for i, kk in k.taps:
        if yy + i < r: continue
        let st = rows.get(yy + i - r, c0, sw)
        if st < 0: continue
        for t in 0 ..< sw: result[yy * sw + t] += kk * rows.tmp[st + t]
  of kCos: result = k.ck.strip(rows, c0, sw)

proc feather*(m: openArray[float32]; w, h: int; radius: float32): seq[float32] =
  ## Select › Modify › Feather: Gaussian blur of the mask with `σ = radius`, the kernel truncated
  ## at `⌈3σ⌉` and normalized, zero beyond the canvas (Photoshop's feather radius is the σ).
  let n = (if w > 0 and h > high(int) div w: high(int) else: w * h)
  var sigma =
    if radius.classify notin {fcNan, fcInf, fcNegInf}: rmax(radius, 0'f32)
    elif radius > 0: MaxSigma
    else: 0'f32
  if sigma < 0.1'f32 or m.len != n or n == 0: return @m
  sigma = min(sigma, MaxSigma)
  let r = asIdx(ceil(sigma * 3'f32))
  let bo = nonzeroBounds(m, w, h)
  if bo.isNone: return @m
  let b = bo.get
  let win = grow(b, r, w, h)
  let (gw, gh) = (win.w, win.h)
  let s = float64(sigma)
  let (kh, kv) = (newKernel(s, r, gw), newKernel(s, r, gh))
  # Rows: only the bounding box's rows are non-zero.
  var tmp = newSeq[float32](b.h * gw)
  var runs: seq[int]
  for y in 0 ..< b.h:
    let o = (b.y0 + y) * w + win.x0
    if o + gw <= m.len and gw > 0:
      kh.line(m.toOpenArray(o, o + gw - 1), tmp.toOpenArray(y * gw, y * gw + gw - 1), runs)
  # Columns, a strip of columns at a time.
  let rows = Rows(tmp: move(tmp), gw: gw, bh: b.h, off: b.y0 - win.y0, gh: gh)
  let res = strips(gw, proc(c0, sw: int): seq[float32] = kv.strip(rows, c0, sw))
  # Rounding residue of the sliding sums is not coverage.
  assemble(res, win, w, h, 0'f32, proc(v: float32): float32 = (if abs(v) < 1e-7'f32: 0'f32 else: v))
