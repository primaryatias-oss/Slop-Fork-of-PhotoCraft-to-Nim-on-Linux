## Hybrid max-flow / min-cut: Boykov–Kolmogorov and FIFO push/relabel (port of
## `segment/maxflow.rs` and `segment/maxflow/push_relabel.rs`).
##
## Y. Boykov and V. Kolmogorov, "An Experimental Comparison of Min-Cut/Max-Flow Algorithms for
## Energy Minimization in Vision", IEEE PAMI 26(9), 2004: two search trees (source tree S, sink
## tree T) grow towards each other; each S–T contact gives an augmenting path; saturated tree
## edges create orphans that are re-adopted (or freed) before growing again.
##
## Large finite graphs with both terminal directions use FIFO push/relabel instead (Goldberg &
## Tarjan, "A New Approach to the Maximum-Flow Problem", JACM 35(4), 1988, with global
## relabelling): excess is discharged locally, with periodic exact distances to both terminals.

import std/[deques, options]
import ./util

const
  NoneArc = high(uint32)
  Terminal = high(uint32) - 1
  Orphan = high(uint32) - 2
  InfDist = high(uint32)
  Eps = 1e-5'f32 ## residuals at or below this count as saturated
  CancelEvery = 1024'u32 ## steps between asks whether to stop

type
  Graph* = object
    ## A directed graph with source/sink terminal capacities.
    first: seq[uint32]
    trCap: seq[float32] ## residual terminal capacity: `> 0` from the source, `< 0` to the sink
    parent: seq[uint32]
    isSink: seq[bool]
    ts: seq[uint32]
    dist: seq[uint32]
    active: seq[bool]
    head: seq[uint32]  ## arcs in pairs: arc `a` and its reverse `a xor 1`
    next: seq[uint32]
    rCap: seq[float32]
    flow: float64

proc initGraph*(nodes, edges: int): Graph =
  ## A graph with `nodes` nodes and room for `edges` undirected edge pairs.
  result = Graph(first: newSeq[uint32](nodes), trCap: newSeq[float32](nodes), parent: newSeq[uint32](nodes),
                 isSink: newSeq[bool](nodes), ts: newSeq[uint32](nodes), dist: newSeq[uint32](nodes),
                 active: newSeq[bool](nodes))
  for v in result.first.mitems: v = NoneArc
  for v in result.parent.mitems: v = NoneArc
  result.head = newSeqOfCap[uint32](edges * 2)
  result.next = newSeqOfCap[uint32](edges * 2)
  result.rCap = newSeqOfCap[float32](edges * 2)

proc nodeCount*(g: Graph): int = g.first.len

proc addTweights*(g: var Graph; i: int; capSource, capSink: float32) =
  ## Adds capacities from the source and to the sink (accumulating). Only the difference is kept
  ## per node; the common part is flow that must pass anyway.
  let delta = g.trCap[i]
  var (s, t) = (capSource, capSink)
  if delta > 0'f32: s += delta
  else: t -= delta
  g.flow += float64(rmin(s, t))
  g.trCap[i] = s - t

proc addEdge*(g: var Graph; i, j: int; cap, revCap: float32) =
  ## Adds an edge `i → j` with capacity `cap` and `j → i` with `revCap`.
  let a = uint32(g.head.len)
  g.head.add uint32(j)
  g.next.add g.first[i]
  g.rCap.add cap
  g.first[i] = a
  g.head.add uint32(i)
  g.next.add g.first[j]
  g.rCap.add revCap
  g.first[j] = a + 1

proc inSource*(g: Graph; i: int): bool =
  ## After `maxflow`: is node `i` on the source side of the minimum cut? Nodes reachable from
  ## neither terminal are reported on the sink side (the smallest source set).
  g.parent[i] != NoneArc and not g.isSink[i]

proc flow*(g: Graph): float64 = g.flow

proc augment(g: var Graph; m: int; orphans: var Deque[uint32]) =
  let u = int(g.head[m xor 1])
  let v = int(g.head[m])
  # Bottleneck.
  var b = g.rCap[m]
  var x = u
  while true:
    let a = g.parent[x]
    if a == Terminal: break
    b = rmin(b, g.rCap[int(a) xor 1])
    x = int(g.head[int(a)])
  b = rmin(b, g.trCap[x])
  x = v
  while true:
    let a = g.parent[x]
    if a == Terminal: break
    b = rmin(b, g.rCap[int(a)])
    x = int(g.head[int(a)])
  b = rmin(b, -g.trCap[x])
  # Push.
  g.rCap[m xor 1] += b
  g.rCap[m] -= b
  x = u
  while true:
    let a = g.parent[x]
    if a == Terminal: break
    let au = int(a)
    g.rCap[au] += b
    g.rCap[au xor 1] -= b
    let up = int(g.head[au])
    if g.rCap[au xor 1] <= Eps:
      g.parent[x] = Orphan
      orphans.addLast uint32(x)
    x = up
  g.trCap[x] -= b
  if g.trCap[x] <= Eps:
    g.parent[x] = Orphan
    orphans.addLast uint32(x)
  x = v
  while true:
    let a = g.parent[x]
    if a == Terminal: break
    let au = int(a)
    g.rCap[au xor 1] += b
    g.rCap[au] -= b
    let up = int(g.head[au])
    if g.rCap[au] <= Eps:
      g.parent[x] = Orphan
      orphans.addLast uint32(x)
    x = up
  g.trCap[x] += b
  if g.trCap[x] >= -Eps:
    g.parent[x] = Orphan
    orphans.addLast uint32(x)
  g.flow += float64(b)

proc adopt(g: var Graph; i: int; time: uint32; queue, orphans: var Deque[uint32]) =
  let sink = g.isSink[i]
  var best = NoneArc
  var dMin = InfDist
  var a = g.first[i]
  while a != NoneArc:
    let au = int(a)
    let cap = (if sink: g.rCap[au] else: g.rCap[au xor 1])
    let j = int(g.head[au])
    if cap > Eps and g.isSink[j] == sink and g.parent[j] != NoneArc:
      # Does j's path lead to a terminal?
      var d = 0'u32
      var k = j
      while true:
        if g.ts[k] == time:
          d = (if InfDist - d < g.dist[k]: InfDist else: d + g.dist[k])
          break
        let pa = g.parent[k]
        d += 1
        if pa == Terminal:
          g.ts[k] = time
          g.dist[k] = 1
          break
        if pa == Orphan:
          d = InfDist
          break
        k = int(g.head[int(pa)])
      if d < InfDist:
        if d < dMin:
          best = a
          dMin = d
        # Cache distances along the path.
        var kk = j
        var dd = d
        while g.ts[kk] != time:
          g.ts[kk] = time
          g.dist[kk] = dd
          dd -= 1
          kk = int(g.head[int(g.parent[kk])])
    a = g.next[au]
  if best != NoneArc:
    g.parent[i] = best
    g.ts[i] = time
    g.dist[i] = dMin + 1
    return
  # No parent: i becomes free.
  g.parent[i] = NoneArc
  a = g.first[i]
  while a != NoneArc:
    let au = int(a)
    let j = int(g.head[au])
    let pj = g.parent[j]
    if g.isSink[j] == sink and pj != NoneArc:
      let cap = (if sink: g.rCap[au] else: g.rCap[au xor 1])
      if cap > Eps and not g.active[j]:
        g.active[j] = true
        queue.addLast uint32(j)
      if pj != Terminal and pj != Orphan and int(g.head[int(pj)]) == i:
        g.parent[j] = Orphan
        orphans.addLast uint32(j)
    a = g.next[au]

proc bk(g: var Graph; cancel: proc(): bool): Option[float64] =
  let n = g.nodeCount
  var queue = initDeque[uint32]()
  var orphans = initDeque[uint32]()
  for i in 0 ..< n:
    g.active[i] = false
    g.ts[i] = 0
    if abs(g.trCap[i]) > Eps:
      g.isSink[i] = g.trCap[i] < 0'f32
      g.parent[i] = Terminal
      g.dist[i] = 1
      g.active[i] = true
      queue.addLast uint32(i)
    else:
      g.parent[i] = NoneArc
  var time = 0'u32
  var steps = 0'u32
  while queue.len > 0:
    let i = int(queue.popFirst)
    steps += 1
    if steps mod CancelEvery == 0 and cancel(): return none(float64)
    g.active[i] = false
    if g.parent[i] == NoneArc: continue
    # Growth stage.
    var contact = NoneArc
    let sink = g.isSink[i]
    var a = g.first[i]
    while a != NoneArc:
      let au = int(a)
      let j = int(g.head[au])
      let cap = (if sink: g.rCap[au xor 1] else: g.rCap[au])
      if cap > Eps:
        if g.parent[j] == NoneArc:
          g.isSink[j] = sink
          g.parent[j] = a xor 1
          g.ts[j] = g.ts[i]
          g.dist[j] = g.dist[i] + 1
          if not g.active[j]:
            g.active[j] = true
            queue.addLast uint32(j)
        elif g.isSink[j] != sink:
          # Arc from the S side to the T side.
          contact = (if sink: a xor 1 else: a)
          break
        elif g.ts[j] <= g.ts[i] and g.dist[j] > g.dist[i]:
          # Heuristic: shorten paths to the terminal.
          g.parent[j] = a xor 1
          g.ts[j] = g.ts[i]
          g.dist[j] = g.dist[i] + 1
      a = g.next[au]
    if contact == NoneArc: continue
    # Keep scanning this node after the augmentation.
    if not g.active[i]:
      g.active[i] = true
      queue.addFirst uint32(i)
    time += 1
    g.augment(int(contact), orphans)
    # Adoption stage.
    while orphans.len > 0:
      let o = int(orphans.popFirst)
      g.adopt(o, time, queue, orphans)
  some(g.flow)

proc globalRelabel(g: var Graph; sourceHeight: uint32; back: seq[float32]) =
  for v in g.dist.mitems: v = high(uint32)
  g.parent = g.first
  var queue = initDeque[int]()
  for i in 0 ..< g.nodeCount:
    if g.trCap[i] < -Eps:
      g.dist[i] = 1
      queue.addLast i
  template sweep() =
    while queue.len > 0:
      let i = queue.popFirst
      var a = g.first[i]
      while a != NoneArc:
        let ai = int(a)
        let j = int(g.head[ai])
        if g.rCap[ai xor 1] > Eps and g.dist[j] == high(uint32):
          g.dist[j] = (if g.dist[i] == high(uint32): high(uint32) else: g.dist[i] + 1)
          queue.addLast j
        a = g.next[ai]
  sweep()
  # Give sink-unreachable vertices exact distances back to the source.
  for i, capacity in back:
    if capacity > Eps and g.dist[i] == high(uint32):
      g.dist[i] = sourceHeight + 1
      queue.addLast i
  sweep()

proc pushRelabel(g: var Graph; cancel: proc(): bool): Option[float64] =
  let n = g.nodeCount
  if n >= int(high(uint32) div 2): return g.bk(cancel)
  let sourceHeight = uint32(n) + 2
  var back = newSeq[float32](n)
  var excess = newSeq[float64](n)
  for i in 0 ..< n:
    g.active[i] = false
    if g.trCap[i] > Eps:
      back[i] = g.trCap[i]
      excess[i] = float64(g.trCap[i])
      g.trCap[i] = 0
  g.globalRelabel(sourceHeight, back)
  var queue = initDeque[int]()
  for i, e in excess:
    if e > float64(Eps):
      g.active[i] = true
      queue.addLast i
  var work = 0
  let frequency = g.rCap.len * 2 + n
  var steps = 0'u32
  template sat1(v: uint32): uint32 = (if v == high(uint32): v else: v + 1)
  while queue.len > 0:
    let i = queue.popFirst
    steps += 1
    if steps mod CancelEvery == 0 and cancel(): return none(float64)
    g.active[i] = false
    while excess[i] > float64(Eps):
      if g.dist[i] == 1 and g.trCap[i] < -Eps:
        let delta = rmin(float32(excess[i]), -g.trCap[i])
        g.trCap[i] += delta
        excess[i] -= float64(delta)
        g.flow += float64(delta)
        if excess[i] <= float64(Eps): break
      if g.dist[i] == sourceHeight + 1 and back[i] > Eps:
        let delta = rmin(float32(excess[i]), back[i])
        back[i] -= delta
        g.trCap[i] += delta
        excess[i] -= float64(delta)
        if excess[i] <= float64(Eps): break
      var a = g.parent[i]
      while a != NoneArc:
        let ai = int(a)
        let j = int(g.head[ai])
        work += 1
        if g.rCap[ai] > Eps and g.dist[i] == sat1(g.dist[j]):
          let delta = rmin(float32(excess[i]), g.rCap[ai])
          g.rCap[ai] -= delta
          g.rCap[ai xor 1] += delta
          excess[i] -= float64(delta)
          excess[j] += float64(delta)
          if not g.active[j] and excess[j] > float64(Eps):
            g.active[j] = true
            queue.addLast j
          if excess[i] <= float64(Eps): break
        a = g.next[ai]
        g.parent[i] = a
      if excess[i] <= float64(Eps): break
      var height = (if g.trCap[i] < -Eps: 0'u32 else: high(uint32))
      if back[i] > Eps: height = min(height, sourceHeight)
      a = g.first[i]
      while a != NoneArc:
        let ai = int(a)
        work += 1
        if g.rCap[ai] > Eps: height = min(height, g.dist[int(g.head[ai])])
        a = g.next[ai]
      if height == high(uint32): break
      g.dist[i] = sat1(height)
      g.parent[i] = g.first[i]
    if work >= frequency:
      g.globalRelabel(sourceHeight, back)
      work = 0
  # Report the smallest source-side cut, independently of discharge order.
  for v in g.parent.mitems: v = NoneArc
  for v in g.isSink.mitems: v = true
  for i in 0 ..< n:
    if g.trCap[i] > Eps:
      g.parent[i] = Terminal
      g.isSink[i] = false
      queue.addLast i
  while queue.len > 0:
    let i = queue.popFirst
    var a = g.first[i]
    while a != NoneArc:
      let ai = int(a)
      let j = int(g.head[ai])
      if g.rCap[ai] > Eps and g.parent[j] == NoneArc:
        g.parent[j] = Terminal
        g.isSink[j] = false
        queue.addLast j
      a = g.next[ai]
  some(g.flow)

proc never(): bool = false

proc maxflowWith*(g: var Graph; cancel: proc(): bool): Option[float64] =
  ## `maxflow` that stops when `cancel` returns true (asked every few thousand steps): none then,
  ## and the cut is meaningless.
  if g.nodeCount >= 4096:
    var (source, sink) = (false, false)
    var finite = true
    for capacity in g.trCap:
      if not capacity.isFinite:
        finite = false
        break
      source = source or capacity > Eps
      sink = sink or capacity < -Eps
    if not finite: return g.bk(cancel)
    if source and sink:
      var ok = true
      for v in g.rCap:
        if not (v.isFinite and v >= 0'f32):
          ok = false
          break
      if ok: return g.pushRelabel(cancel)
  g.bk(cancel)

proc maxflow*(g: var Graph): float64 =
  ## Computes the maximum flow (= minimum cut capacity).
  let r = g.maxflowWith(never)
  if r.isSome: r.get else: g.flow

proc maxflowBk*(g: var Graph): float64 =
  ## Boykov–Kolmogorov only.
  let r = g.bk(never)
  if r.isSome: r.get else: g.flow

proc maxflowPushRelabel*(g: var Graph): float64 =
  ## Push/relabel only (for tests).
  let r = g.pushRelabel(never)
  if r.isSome: r.get else: g.flow

proc residualOutOfSourceSide*(g: Graph): float32 =
  ## Sum of residual capacities on arcs leaving the source side of the cut (zero after `maxflow`:
  ## no augmenting path remains).
  for i in 0 ..< g.nodeCount:
    if g.inSource(i):
      var a = g.first[i]
      while a != NoneArc:
        let j = int(g.head[int(a)])
        if not g.inSource(j): result += g.rCap[int(a)]
        a = g.next[int(a)]
