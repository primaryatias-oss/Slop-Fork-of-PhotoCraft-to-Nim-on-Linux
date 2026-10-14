## Unicode Bidirectional Algorithm (UAX #9): embedding levels for a paragraph, a port of parley's
## resolver (explicit embeddings and isolates, weak and neutral types, bracket pairs, L1).

import std/algorithm
import uprops

const
  MaxStack = 125
  MaxBracketStack = 63

proc bit(t: BidiClass): uint32 {.inline.} = 1'u32 shl ord(t)

const
  OverrideMask = bit(bcRLE) or bit(bcLRE) or bit(bcRLO) or bit(bcLRO)
  IsolateMask = bit(bcRLI) or bit(bcLRI) or bit(bcFSI)
  ExplicitMask = OverrideMask or IsolateMask
  RtlMask = bit(bcRLE) or bit(bcRLO) or bit(bcRLI)
  RemovedByX9Mask = OverrideMask or bit(bcPDF) or bit(bcBN)
  BidiMask = ExplicitMask or bit(bcR) or bit(bcAL) or bit(bcAN)

proc needsBidiResolution*(t: BidiClass): bool = (bit(t) and BidiMask) != 0
proc isRemovedByX9(t: BidiClass): bool = (bit(t) and RemovedByX9Mask) != 0
proc isIsolateInitiator(t: BidiClass): bool = (bit(t) and IsolateMask) != 0
proc typeFromLevel(level: int): BidiClass = (if (level and 1) == 0: bcL else: bcR)

type
  Run = object
    level: int
    endsWithIsolate, startsWithPdi: bool
    sos, eos: BidiClass
    start, stop: int
    inSequence: bool
    next: int

  BidiResolver* = object
    baseLevel*: int
    levels*: seq[uint8]
    initialTypes: seq[BidiClass]
    types: seq[BidiClass]
    brackets: seq[(int, int, BracketType, int)]   ## (char index, char, type, mirror)
    bracketPairs: seq[(int, int)]
    runs: seq[Run]
    indices: seq[int]

proc defaultLevel(types: openArray[BidiClass]; untilPdi = false): int =
  var isolates = 0
  for t in types:
    case t
    of bcRLI, bcLRI, bcFSI: inc isolates
    of bcPDI:
      if isolates > 0: dec isolates
      elif untilPdi: return 0
    of bcL, bcR, bcAL:
      if isolates == 0: return (if t == bcL: 0 else: 1)
    else: discard
  0

type Stack = object
  levels: array[MaxStack + 1, int]
  override: array[MaxStack + 1, BidiClass]
  isolate: array[MaxStack + 1, bool]
  depth: int

proc push(s: var Stack; level: int; o: BidiClass; iso: bool) =
  if s.depth > MaxStack: return
  s.levels[s.depth] = level
  s.override[s.depth] = o
  s.isolate[s.depth] = iso
  inc s.depth

proc pop(s: var Stack) =
  if s.depth > 1: dec s.depth

proc level(s: Stack): int = s.levels[s.depth - 1]
proc overrideStatus(s: Stack): BidiClass = s.override[s.depth - 1]
proc isolateStatus(s: Stack): bool = s.isolate[s.depth - 1]

proc resolveLevels(r: var BidiResolver) =
  let base = r.baseLevel
  let n = r.types.len
  r.levels = newSeq[uint8](n)
  var stack: Stack
  var overflowIsolates, overflowEmbedding, validIsolates = 0
  stack.push(base, bcON, false)
  for i in 0 ..< n:
    let t = r.types[i]
    let tm = bit(t)
    if (tm and ExplicitMask) != 0:
      let isIsolate = (tm and IsolateMask) != 0
      let isRtl = if t == bcFSI and i + 1 < n: defaultLevel(r.types.toOpenArray(i + 1, n - 1), true) == 1
                  else: (tm and RtlMask) != 0
      if isIsolate:
        r.levels[i] = uint8(stack.level)
        let os = stack.overrideStatus
        if os != bcON: r.types[i] = os
      let newLevel = if isRtl: (stack.level + 1) or 1 else: (stack.level + 2) and not 1
      if newLevel <= MaxStack and overflowIsolates == 0 and overflowEmbedding == 0:
        if isIsolate: inc validIsolates
        stack.push(newLevel, (if t == bcLRO: bcL elif t == bcRLO: bcR else: bcON), isIsolate)
      elif isIsolate: inc overflowIsolates
      elif overflowIsolates == 0: inc overflowEmbedding
    elif t == bcPDI:
      if overflowIsolates > 0: dec overflowIsolates
      elif validIsolates == 0: discard
      else:
        overflowEmbedding = 0
        while not stack.isolateStatus and stack.depth > 1: stack.pop()
        stack.pop()
        dec validIsolates
      r.levels[i] = uint8(stack.level)
      if stack.overrideStatus != bcON: r.types[i] = stack.overrideStatus
    elif t == bcPDF:
      r.levels[i] = uint8(stack.level)
      if overflowIsolates > 0: discard
      elif overflowEmbedding > 0: dec overflowEmbedding
      elif not stack.isolateStatus and stack.depth >= 2: stack.pop()
    elif t == bcB:
      stack.depth = 1
      overflowIsolates = 0
      overflowEmbedding = 0
      validIsolates = 0
      r.levels[i] = uint8(base)
    elif t != bcBN:
      r.levels[i] = uint8(stack.level)
      if stack.overrideStatus != bcON: r.types[i] = stack.overrideStatus

proc resolveRuns(r: var BidiResolver) =
  let n = r.types.len
  r.runs.setLen(0)
  var start = 0
  while start < n and isRemovedByX9(r.types[start]): inc start
  if start == n: return
  var level = int(r.levels[start])
  var offset = 0
  for i in start + 1 ..< n:
    if isRemovedByX9(r.types[i]): continue
    if int(r.levels[i]) != level:
      r.runs.add Run(level: level, start: offset, stop: i, sos: bcON, eos: bcON, next: -1)
      offset = i
      level = int(r.levels[i])
  if offset < n: r.runs.add Run(level: level, start: offset, stop: n, sos: bcON, eos: bcON, next: -1)
  for run in r.runs.mitems:
    while run.start < run.stop and isRemovedByX9(r.types[run.start]): inc run.start
    while run.stop > run.start and isRemovedByX9(r.types[run.stop - 1]): dec run.stop
    if run.start == run.stop: continue
    if r.types[run.start] == bcPDI: run.startsWithPdi = true
    var prevLevel = r.baseLevel
    for i in countdown(run.start - 1, 0):
      if not isRemovedByX9(r.types[i]):
        prevLevel = int(r.levels[i])
        break
    run.sos = typeFromLevel(max(prevLevel, run.level))
    if isIsolateInitiator(r.initialTypes[run.stop - 1]):
      run.endsWithIsolate = true
      run.eos = typeFromLevel(max(r.baseLevel, run.level))
    else:
      var nextLevel = r.baseLevel
      for i in run.stop ..< n:
        if not isRemovedByX9(r.types[i]):
          nextLevel = int(r.levels[i])
          break
      run.eos = typeFromLevel(max(nextLevel, run.level))
  for i in 0 ..< r.runs.len:
    if r.runs[i].endsWithIsolate:
      let level = r.runs[i].level
      for j in i + 1 ..< r.runs.len:
        if r.runs[j].startsWithPdi and r.runs[j].level == level:
          r.runs[i].next = j
          r.runs[j].inSequence = true
          break

proc findLimit(types: openArray[BidiClass]; offset: int; t: BidiClass): int =
  result = offset
  while result < types.len and types[result] == t: inc result

proc findLimitByMask(types: openArray[BidiClass]; offset: int; m: uint32): int =
  result = offset
  while result < types.len and (bit(types[result]) and m) != 0: inc result

proc strongDir(t: BidiClass): BidiClass =
  case t
  of bcEN, bcAN, bcAL, bcR: bcR
  of bcL: bcL
  else: bcON

proc resolveSequence(r: var BidiResolver; level: int; sos, eos: BidiClass; len: int) =
  if len == 0: return
  let base0 = r.initialTypes.len
  template types(i: int): untyped = r.types[base0 + i]
  const
    W1Mask = bit(bcLRI) or bit(bcRLI) or bit(bcFSI) or bit(bcPDI)
    W2Mask = bit(bcL) or bit(bcR) or bit(bcAL)
    W4Mask = bit(bcES) or bit(bcCS)
  var prev = sos
  var prevStrong = prev
  for i in 0 ..< len:
    var t = types(i)
    let tm = bit(t)
    if t == bcNSM:
      types(i) = prev
    else:
      if (tm and W1Mask) != 0:
        prev = bcON
        continue
      if t == bcEN:
        if prevStrong == bcAL:
          t = bcAN
          types(i) = t
      elif (tm and W2Mask) != 0:
        prevStrong = t
        if t == bcAL:
          t = bcR
          types(i) = t
      elif (tm and W4Mask) != 0 and i < len - 1:
        var nxt = types(i + 1)
        if nxt == bcEN and prevStrong == bcAL: nxt = bcAN
        if prev == bcEN and nxt == bcEN:
          t = bcEN
          types(i) = t
        elif t == bcCS and prev == bcAN and nxt == bcAN:
          t = bcAN
          types(i) = t
      prev = t
  var i = 0
  while i < len:
    if types(i) == bcET:
      var limit = i
      while limit < len and types(limit) == bcET: inc limit
      var t = if i == 0: sos else: types(i - 1)
      if t != bcEN: t = if limit == len: eos else: types(limit)
      if t == bcEN:
        for j in i ..< limit: types(j) = bcEN
      i = limit
    inc i
  const W6Mask = bit(bcES) or bit(bcET) or bit(bcCS)
  prevStrong = sos
  for i in 0 ..< len:
    let t = types(i)
    if (bit(t) and W6Mask) != 0: types(i) = bcON
    elif t == bcEN:
      if prevStrong == bcL: types(i) = bcL
    elif t == bcL or t == bcR: prevStrong = t
  # N0: bracket pairs.
  if r.brackets.len > 0:
    let basePairs = r.bracketPairs.len
    var stack: seq[(int, int)]   # (sequence position, expected closer)
    block scan:
      for i in 0 ..< len:
        if types(i) != bcON: continue
        let index = r.indices[i]
        var lo = 0
        var hi = r.brackets.len - 1
        var found = -1
        while lo <= hi:
          let mid = (lo + hi) div 2
          if r.brackets[mid][0] == index:
            found = mid
            break
          if r.brackets[mid][0] < index: lo = mid + 1 else: hi = mid - 1
        if found < 0: continue
        let (_, ch, bt, mir) = r.brackets[found]
        case bt
        of btOpen:
          if stack.len == MaxBracketStack: break scan
          stack.add (i, mir)
        of btClose:
          var k = stack.len - 1
          while k >= 0:
            let c = stack[k][1]
            if c == ch or (c == 0x232A and ch == 0x3009) or (c == 0x3009 and ch == 0x232A):
              r.bracketPairs.add (stack[k][0], i)
              stack.setLen(k)
              break
            dec k
        else: discard
    if r.bracketPairs.len > basePairs:
      let embedDir = if (level and 1) != 0: bcR else: bcL
      var pairs = r.bracketPairs[basePairs .. ^1]
      pairs.sort(proc(a, b: (int, int)): int = cmp(a[0], b[0]))
      for k in 0 ..< pairs.len:
        let (o, c) = pairs[k]
        var pairDir = bcON
        for i in o + 1 ..< c:
          let d = strongDir(types(i))
          if d == bcON: continue
          pairDir = d
          if d == embedDir: break
        if pairDir == bcON:
          pairs[k] = (r.indices[o], r.indices[c])
          continue
        if pairDir != embedDir:
          pairDir = sos
          for i in countdown(o - 1, 0):
            let d = strongDir(types(i))
            if d != bcON:
              pairDir = d
              break
          if pairDir == embedDir or pairDir == bcON: pairDir = embedDir
        types(o) = pairDir
        types(c) = pairDir
        for i in o + 1 ..< c:
          if r.initialTypes[r.indices[i]] == bcNSM: types(i) = pairDir
          else: break
        for i in c + 1 ..< len:
          if r.initialTypes[r.indices[i]] == bcNSM: types(i) = pairDir
          else: break
        pairs[k] = (r.indices[o], r.indices[c])
      for k in 0 ..< pairs.len: r.bracketPairs[basePairs + k] = pairs[k]
  # N1, N2.
  const NMask = bit(bcB) or bit(bcS) or bit(bcWS) or bit(bcON) or bit(bcRLI) or bit(bcLRI) or
                bit(bcFSI) or bit(bcPDI)
  i = 0
  while i < len:
    let t = types(i)
    if (bit(t) and NMask) != 0:
      let offset = i
      var limit = offset
      while limit < len and (bit(types(limit)) and NMask) != 0: inc limit
      var leading = if offset == 0: sos else: types(offset - 1)
      if offset > 0 and leading in {bcAN, bcEN}: leading = bcR
      var trailing = if limit == len: eos else: types(limit)
      if limit < len and trailing in {bcAN, bcEN}: trailing = bcR
      let resolved = if leading == trailing: leading elif (level and 1) != 0: bcR else: bcL
      for j in offset ..< limit: types(j) = resolved
      i = limit - 1
    inc i
  # I1, I2.
  if (level and 1) == 0:
    for i in 0 ..< len:
      let index = r.indices[i]
      let t = types(i)
      r.levels[index] = uint8(if t == bcR: level + 1 elif t != bcL: level + 2 else: level)
  else:
    for i in 0 ..< len:
      let index = r.indices[i]
      r.levels[index] = uint8(if types(i) != bcR: level + 1 else: level)

proc resolve*(r: var BidiResolver; chars: openArray[int]; baseLevel = -1) =
  ## Resolves embedding levels for `chars` (one paragraph). `baseLevel` < 0: from the first
  ## strong character (P2/P3).
  r.initialTypes.setLen(0)
  r.levels.setLen(0)
  r.types.setLen(0)
  r.brackets.setLen(0)
  r.bracketPairs.setLen(0)
  var needs = false
  for i, ch in chars:
    let t = bidiClass(ch)
    r.initialTypes.add t
    let bt = bracketType(ch)
    if bt != btNone: r.brackets.add (i, ch, bt, mirror(ch))
    needs = needs or needsBidiResolution(t)
  let n = chars.len
  r.baseLevel = if baseLevel >= 0: baseLevel and 1 else: defaultLevel(r.initialTypes)
  if not needs and r.baseLevel == 0:
    r.levels = newSeq[uint8](n)
    return
  r.types = r.initialTypes
  r.resolveLevels()
  r.resolveRuns()
  for i in 0 ..< r.runs.len:
    if r.runs[i].inSequence: continue
    r.types.setLen(n)
    r.indices.setLen(0)
    var cur = i
    let level = r.runs[i].level
    let sos = r.runs[i].sos
    var eos: BidiClass
    while true:
      let run = r.runs[cur]
      for k in run.start ..< run.stop:
        let ty = r.types[k]
        if not isRemovedByX9(ty):
          r.types.add ty
          r.indices.add k
      eos = run.eos
      if run.next < 0: break
      cur = run.next
    r.resolveSequence(level, sos, eos, r.indices.len)
  for i in 0 ..< n:
    let t = r.initialTypes[i]
    if t == bcS or t == bcB:
      r.levels[i] = uint8(r.baseLevel)
      for j in countdown(i - 1, 0):
        let tj = r.initialTypes[j]
        if isRemovedByX9(tj): continue
        elif tj == bcWS or isIsolateInitiator(tj) or tj == bcPDI: r.levels[j] = uint8(r.baseLevel)
        else: break
    elif isRemovedByX9(t):
      r.levels[i] = (if i == 0: uint8(r.baseLevel) else: r.levels[i - 1])

proc reorderLevels*(levels: openArray[uint8]): seq[int] =
  ## Visual order (L2) of items with the given levels: `result[k]` is the logical index shown k-th.
  let n = levels.len
  result = newSeq[int](n)
  for i in 0 ..< n: result[i] = i
  var maxLevel = 0
  var lowestOdd = 255
  for l in levels:
    maxLevel = max(maxLevel, int(l))
    if (int(l) and 1) != 0: lowestOdd = min(lowestOdd, int(l))
  for level in countdown(maxLevel, lowestOdd):
    var i = 0
    while i < n:
      if int(levels[result[i]]) >= level:
        var e = i + 1
        while e < n and int(levels[result[e]]) >= level: inc e
        var j = i
        var k = e - 1
        while j < k:
          swap(result[j], result[k])
          inc j
          dec k
        i = e
      inc i
