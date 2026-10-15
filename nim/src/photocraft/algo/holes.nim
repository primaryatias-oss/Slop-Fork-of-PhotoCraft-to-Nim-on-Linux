## Hole-mask helpers of the Remove Tool (`remove.rs`): enclosed areas, dilation and thickness.

proc closeLoops*(w, h: int; mask: openArray[bool]): seq[bool] =
  ## `mask` plus everything it encloses: the cells from which no path of unmasked cells
  ## (4-connected) reaches the grid's border. A mask of the wrong length is returned unchanged.
  if w <= 0 or h <= 0 or w > high(int) div h or mask.len != w * h: return @mask
  var outside = newSeq[bool](mask.len)
  var stack: seq[int]
  template visit(i: int) =
    if not mask[i] and not outside[i]:
      outside[i] = true
      stack.add i
  for x in 0 ..< w:
    visit(x)
    visit((h - 1) * w + x)
  for y in 0 ..< h:
    visit(y * w)
    visit(y * w + w - 1)
  while stack.len > 0:
    let i = stack.pop()
    let (x, y) = (i mod w, i div w)
    if x > 0: visit(i - 1)
    if x + 1 < w: visit(i + 1)
    if y > 0: visit(i - w)
    if y + 1 < h: visit(i + w)
  result = newSeq[bool](mask.len)
  for i, o in outside: result[i] = not o

proc dilate*(w, h: int; mask: openArray[bool]; r: int): seq[bool] =
  ## `mask` grown by `r` cells (a square structuring element), in two separable passes.
  if r <= 0 or w < 0 or h < 0 or (w > 0 and h > high(int) div w) or mask.len != w * h: return @mask
  proc pass(src: seq[bool]; horizontal: bool): seq[bool] =
    let (lines, len) = (if horizontal: (h, w) else: (w, h))
    template at(line, k: int): int = (if horizontal: line * w + k else: k * w + line)
    result = newSeq[bool](src.len)
    for line in 0 ..< lines:
      var count = 0
      for k in 0 ..< min(len, r): count += ord(src[at(line, k)])
      for k in 0 ..< len:
        if k + r < len: count += ord(src[at(line, k + r)])
        if k > r: count -= ord(src[at(line, k - r - 1)])
        result[at(line, k)] = count > 0
  pass(pass(@mask, true), false)

proc holeRadius*(w, h: int; hole: openArray[bool]): uint32 =
  ## The largest chessboard distance from a hole cell to the nearest cell outside the hole (cells
  ## beyond the grid don't count as outside). `high(uint32)` when no cell lies outside the hole, 0
  ## for a mask of the wrong size.
  if w < 0 or h < 0 or (w > 0 and h > high(int) div w) or hole.len != w * h: return 0
  var d = newSeq[uint32](hole.len)
  for i, m in hole: d[i] = (if m: high(uint32) else: 0'u32)
  template relax(i, j: int) =
    let v = (if d[j] == high(uint32): high(uint32) else: d[j] + 1)
    if v < d[i]: d[i] = v
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      if x > 0: relax(i, i - 1)
      if y > 0:
        relax(i, i - w)
        if x > 0: relax(i, i - w - 1)
        if x + 1 < w: relax(i, i - w + 1)
  for y in countdown(h - 1, 0):
    for x in countdown(w - 1, 0):
      let i = y * w + x
      if x + 1 < w: relax(i, i + 1)
      if y + 1 < h:
        relax(i, i + w)
        if x + 1 < w: relax(i, i + w + 1)
        if x > 0: relax(i, i + w - 1)
  for i, m in hole:
    if m: result = max(result, d[i])
