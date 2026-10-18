## Brush engine commands of the algo cross-check (the Nim twin of the oracle's `ax_paint.rs`);
## included by `algocheck.nim`, whose helpers it uses.

const PtArea = rect(-12, -12, 112, 84) ## the area every surface result is dumped over

proc ptRect(r: Rect): string = $r.x0 & " " & $r.y0 & " " & $r.x1 & " " & $r.y1

proc ptBrush(s: string): BrushSettings = brushFromJson(parseJson(s))

proc ptPoints(s: string): seq[StrokePoint] = fromJson(parseJson(s), seq[StrokePoint])

proc ptTarget(f, img: string): Surface =
  if img == "-": newSurface(fmtArg(f)) else: surfaceArg(f, readBuf(img))

proc dabLine(d: Dab): string =
  @[b64(d.center.x), b64(d.center.y), bitsStr(d.radius), bitsStr(d.alpha), bitsStr(d.angle),
    bitsStr(d.roundness), $ord(d.flipX), $ord(d.flipY), bitsStr(d.opacity), bitsStr(d.color[0]),
    bitsStr(d.color[1]), bitsStr(d.color[2]), bitsStr(d.color[3]), bitsStr(d.depth), $d.index,
    bitsStr(d.projAngle), bitsStr(d.projScale), bitsStr(d.wet), bitsStr(d.mix)].join(" ")

proc tileOf(kind: string; n, seed, extra: uint32): GrayTile =
  case kind
  of "chalk": chalkTip(n, seed)
  of "spatter": spatterTip(n, seed, extra)
  of "bristle": bristleTip(n, seed)
  of "charcoal": charcoalTip(n, seed)
  of "leaf": leafTip(n)
  of "grass": grassTip(n, seed, extra)
  of "sponge": spongeTip(n, seed)
  of "star": starTip(n)
  else: rakeTip(n, seed, extra)

proc patternStyleArg(s: string): PatternStyle =
  case s
  of "noise": psNoise
  of "canvas": psCanvas
  of "dots": psDots
  else: psPaper

proc chunks[T](v: seq[T]; n: int): seq[seq[T]] =
  var i = 0
  while i < v.len:
    result.add v[i ..< min(i + n, v.len)]
    i += n

proc paintCmd(a: seq[string]): bool =
  let outp = a[^1]
  case a[1]
  of "pt.rng":
    let (seed, n) = (parseBiggestUInt(a[2]).uint64, parseBiggestUInt(a[3]).uint64)
    for k in 0'u64 ..< n:
      echo bitsStr(rand01(seed, k, k mod 21)), " ", bitsStr(randSigned(seed xor 0x55, k * 3, 7)), " ",
        bitsStr(hash2(int(k) * 7 - 50, int(k) * -13 + 90, seed)), " ", mix64(seed + k)
    echo seedFromBytes(a[2])
  of "pt.misc":
    for k in 0 ..< 60:
      let d = float32(k) * 0.37'f32 - 2'f32
      let (r, h) = (float32(k mod 7) * 1.3'f32 + 0.2'f32, float32(k mod 5) * 0.25'f32)
      let (gx, gy) = gridCenter(float64(d) * 3.1, -float64(d) * 1.7, r * 2'f32)
      let sq = gridSquare(float64(d) * 3.1, float64(d), r * 2'f32 - 1'f32)
      echo bitsStr(tipFalloff(abs(d), r, h)), " ", bitsStr(halfCoverageRadius(r, h)), " ",
        bitsStr(dabCoverage(abs(d), r, h)), " ", bitsStr(gx), " ", bitsStr(gy), " ", b64(sq[0]), " ",
        b64(sq[1]), " ", b64(sq[2]), " ", b64(sq[3])
      let (v, t, dd) = (float32(k mod 11) * 0.1'f32, float32(k mod 13) / 12'f32, float32(k mod 4) / 3'f32)
      var line: seq[string]
      for m in MaskMode: line.add bitsStr(maskCombine(m, v, t, dd))
      echo line.join(" ")
    echo bitsStr(halfCoverageRadius(10, NaN))
    let g = gridCenter(1.2, 3.7, NaN)
    echo bitsStr(g[0]), " ", bitsStr(g[1])
    echo gridSquare(1.2, 3.7, Inf).mapIt(b64(it)).join(" ")
    for p in brush.smooth(ptPoints(a[2]), f(a[3])):
      echo b64(p.x), " ", b64(p.y), " ", bitsStr(p.pressure)
  of "pt.pattern":
    let p = pattern(patternStyleArg(a[2]), uint32(parseInt(a[3])), uint32(parseInt(a[4])))
    writeF32(outp, p.width, p.height, 1, p.data)
    echo p.width, " ", p.height
  of "pt.tip":
    let t = tileOf(a[2], uint32(parseInt(a[3])), uint32(parseInt(a[4])), uint32(parseInt(a[5])))
    writeF32(outp, int(t.width), int(t.height), 1, t.toF32)
    let p = t.preview(uint32(parseInt(a[6])))
    var pv: seq[string]
    for v in p.data[0 ..< min(40, p.data.len)]: pv.add $v
    echo t.width, " ", t.height, " preview ", p.width, " ", p.height, " ", pv.join(",")
    let m = newMips(t)
    var lv: seq[string]
    for L in m.levels: lv.add $L.w & "x" & $L.h & ":" & bitsStr(L.v[L.v.len div 2])
    echo lv.join(" ")
    for k in 0 ..< 20:
      let px = float32(k) * 3.7'f32 + 0.5'f32
      let l = m.levelFor(px)
      echo l, " ", bitsStr(m.sampleClamped(l, float32(k) * 0.061'f32 - 0.1'f32, 1.05'f32 - float32(k) * 0.057'f32))
  of "pt.brushjson":
    try:
      let b = brushFromJson(parseJson(a[2]))
      echo $toJ(b)
      echo $toJ(b.boundedForRender)
    except CatchableError:
      echo "ERR"
  of "pt.presets":
    echo $toJ(builtin())
  of "pt.dabs":
    let st = Stroke(brush: ptBrush(a[2]), points: ptPoints(a[3]))
    let chunk = max(parseInt(a[4]), 1)
    for d in dabs(st): echo dabLine(d)
    var g = newDabGenerator(st.brush, f(a[5]))
    var ds, dual: seq[Dab]
    for c in chunks(st.points, chunk):
      g.push(c, ds, dual)
      echo "chunk ", ds.len, " ", dual.len, " ", g.wantsTime
      let s = g.smoothingString
      if s.isSome:
        let (p, q) = s.get
        echo "string ", b64(p[0]), " ", b64(p[1]), " ", b64(q[0]), " ", b64(q[1])
    g.finish(ds, dual)
    echo "total ", ds.len, " ", dual.len
    for d in dual: echo "dual ", dabLine(d)
  of "pt.render":
    var s = ptTarget(a[2], a[3])
    let sel = maskArg(a[6], rect(0, 0, 97, 71))
    let d = renderStroke(s, ptBrush(a[4]), ptPoints(a[5]), sel, bl(a[7]), f(a[8]))
    echo ptRect(d)
    dumpSurface(outp, s, PtArea)
  of "pt.chunked":
    var s = ptTarget(a[2], a[3])
    let pre = s
    let pts = ptPoints(a[5])
    var r = newStrokeRenderer(ptBrush(a[4]), some(s.format), 1).recordDabs
    for c in chunks(pts, max(parseInt(a[6]), 1)):
      r.push(c)
      let d = r.composite(pre, s, none(Surface), false, false)
      echo "chunk ", ptRect(d), " ", ptRect(r.bounds), " ", r.dabCount
      var t = r.tailPreview
      if t.isSome:
        var s2 = s
        let d2 = t.get.composite(pre, s2, none(Surface), false, false)
        echo "tail ", ptRect(d2), " ", r.tailDabs.len, " ", bitsStr(s2.pixel(40, 30)[0])
    r.markDirty(rect(0, 0, 50, 50))
    echo "dirty ", ptRect(r.takeDirtyRect)
    r.finish()
    let d = r.composite(pre, s, none(Surface), false, false)
    echo "final ", ptRect(d), " ", r.dabs.len
    let cov = r.coverageIn(rect(10, 10, 40, 30))
    var cs: seq[string]
    var k = 0
    while k < cov.len:
      cs.add bitsStr(cov[k])
      k += 7
    echo cs.join(" ")
    dumpSurface(outp, s, PtArea)
  of "pt.coverage":
    let st = Stroke(brush: ptBrush(a[2]), points: ptPoints(a[3]))
    let (r, cov) = strokeCoverage(st)
    echo ptRect(r)
    writeF32(outp, r.width, r.height, 1, cov)
  of "pt.mixer":
    var s = ptTarget(a[2], a[3])
    let st = Stroke(brush: ptBrush(a[4]), points: ptPoints(a[5]))
    let m = fromJson(parseJson(a[6]), MixerSettings)
    var state = fromJson(parseJson(a[7]), MixerState)
    let sample = if a[8] != "-": some(surfaceArg("rgb32", readBuf(a[8]))) else: none(Surface)
    let d = applyMixerStroke(s, sample, st, m, state, none(Surface), bl(a[9]))
    proc opt(v: Option[array[4, float32]]): string =
      if v.isNone: "none" else: v.get.mapIt(bitsStr(it)).join(",")
    echo ptRect(d), " ", bitsStr(state.amount), " ", opt(state.reservoir), " ", opt(state.pickup)
    dumpSurface(outp, s, PtArea)
  of "pt.replace":
    var s = ptTarget(a[2], a[3])
    let st = Stroke(brush: ptBrush(a[4]), points: ptPoints(a[5]))
    let rs = fromJson(parseJson(a[6]), ReplaceSettings)
    let d = applyColorReplacement(s, st, rs, none(Surface), bl(a[7]))
    echo ptRect(d)
    dumpSurface(outp, s, PtArea)
  of "pt.bgerase":
    var s = ptTarget(a[2], a[3])
    let st = Stroke(brush: ptBrush(a[4]), points: ptPoints(a[5]))
    let bs = fromJson(parseJson(a[6]), BgEraseSettings)
    let sel = maskArg(a[7], rect(0, 0, 97, 71))
    let d = applyBackgroundEraser(s, st, bs, sel)
    echo ptRect(d)
    dumpSurface(outp, s, PtArea)
  of "pt.smudge":
    var s = ptTarget(a[2], a[3])
    var st = Stroke(brush: ptBrush(a[4]), points: ptPoints(a[5]))
    st.brush.opacity = f(a[9])
    let finger = if a[7] != "-": some(floats(a[7])) else: none(seq[float32])
    var sm = newSmudge(f(a[6]), finger, st.brush.size)
    if bl(a[8]): sm = sm.withAlpha(alphaIndex(s.format))
    let d = applyDabStroke(s, st, none(Surface), false, i(a[10]), proc(w: var PxRegion; fp: Footprint) = sm.dab(w, fp))
    echo ptRect(d)
    dumpSurface(outp, s, PtArea)
  of "pt.retouch":
    var s = ptTarget(a[2], a[3])
    let st = Stroke(brush: ptBrush(a[4]), points: ptPoints(a[5]))
    let (r, cov) = strokeCoverage(st)
    let fm = s.format
    var paint = newPxRegion(r, fm.channels)
    let n = fm.channels
    for k in 0 ..< paint.data.len:
      paint.data[k] = rmin(float32(k * 37 mod 101) / 100'f32, 1'f32)
      if fm.alpha and k mod n == n - 1: paint.data[k] = 0.75'f32
    let d = applyCoverage(s, r, cov, f(a[6]), none(Surface), bl(a[8]), paint, blendArg(a[7]))
    let ds = dabs(st)
    echo ptRect(d), " ", ptRect(strokeBounds(ds))
    for k in 0 ..< min(3, ds.len):
      let fp = footprint(ds[k], k, st.brush.hardness)
      var v: seq[string]
      var j = 0
      while j < fp.cov.len:
        v.add bitsStr(fp.cov[j])
        j += 5
      echo ptRect(fp.rect), " ", v.join(" ")
    dumpSurface(outp, s, PtArea)
  of "pt.mixpm":
    let v = floats(a[2])
    var d = v[0 .. 3]
    let src = v[4 .. 7]
    mixPremultiplied(d, src, i(a[3]), f(a[4]))
    echo d.mapIt(bitsStr(it)).join(" ")
    var e = v[0 .. 3]
    overNative(pixelFormat(cmRgb, stF32, true), e, src, f(a[4]), bl(a[5]))
    echo e.mapIt(bitsStr(it)).join(" ")
  of "pt.outline":
    let o = tipOutline(ptBrush(a[2]), bl(a[3]))
    if o.isNone: echo "none"
    else:
      for r in o.get: echo r.mapIt(bitsStr(it[0]) & "," & bitsStr(it[1])).join(" ")
  of "pt.sym":
    let c = [parseFloat(a[3]), parseFloat(a[4])]
    let s = newPresetSymmetry(symmetryFromId(a[2]).get(symVertical), c, parseFloat(a[5]))
    if s.isNone: echo "none"
    else:
      let sy = s.get
      echo sy.mirrorCount, " ", b64(sy.rotation)
      for d in sy.directions: echo "dir ", b64(d[0]), " ", b64(d[1])
      for seg in sy.segments([parseFloat(a[6]), parseFloat(a[7])]):
        echo "seg ", b64(seg[0][0]), " ", b64(seg[0][1]), " ", b64(seg[1][0]), " ", b64(seg[1][1])
      for pass in sy.reflectedPasses(ptPoints(a[8])):
        echo pass.mapIt(b64(it.x) & "," & b64(it.y)).join(" ")
  of "pt.union":
    var s = ptTarget(a[2], a[3])
    let pre = s
    let b = ptBrush(a[4])
    let pts = ptPoints(a[5])
    let sym = newPresetSymmetry(symmetryFromId(a[6]).get(symVertical), [float64(f(a[7])), float64(f(a[8]))], 0).get
    var main = newStrokeRenderer(b, some(s.format), 1)
    main.push(pts)
    main.finish()
    var others: seq[StrokeRenderer]
    for p in sym.reflectedPasses(pts):
      var r = newStrokeRenderer(b, some(s.format), 1)
      r.push(p)
      r.finish()
      others.add r
    let d = main.compositeUnionMany(others, pre, s, none(Surface), false)
    echo ptRect(d)
    dumpSurface(outp, s, PtArea)
  of "pt.ctx":
    let b = ptBrush(a[2])
    let ctx = newBrushContext(b)
    let v = floats(a[3])
    var d = roundDab(point(float64(v[0]), float64(v[1])), v[2], v[3])
    d.angle = v[4]
    d.roundness = v[5]
    d.flipX = v[6] > 0.5'f32
    d.projAngle = v[7]
    d.projScale = v[8]
    d.depth = v[9]
    for dual in [false, true]:
      let r = ctx.dabRect(d, dual)
      var cov: seq[float32]
      ctx.rasterize(d, dual, r, cov)
      let fp = ctx.footprint(d, 3)
      echo ptRect(r), " ", cov.len, " ", ptRect(fp.rect)
      echo cov.mapIt(bitsStr(it)).join(" ")
    for k in 0 ..< 30:
      let (x, y) = (k * 7 - 40, k * 5 - 31)
      echo bitsStr(ctx.textureAt(x, y)), " ", bitsStr(ctx.strokeMask(0.6, some(0.3'f32), x, y))
  else: return false
  true
