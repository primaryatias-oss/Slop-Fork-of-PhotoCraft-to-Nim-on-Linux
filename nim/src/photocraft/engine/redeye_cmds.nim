## Red Eye tool (port of `redeye_cmds.rs`): click a pupil to neutralize the flash reflection.
##
## Toolbar-only (`paint.redEye`, empty menu path). The kernel in `algo/redeye` runs on a window
## around each click plus the mask-filter halo.

import std/[json, options, math]
import ./core
import ../algo/redeye as aredeye

const
  RedEyeCmd = "paint.redEye"
  Miss = "no red-eye pixels near the click"

proc rbad(msg: string): ref EngineError = bad(RedEyeCmd, msg)

proc isFin(v: float64): bool {.inline.} = v == v and abs(v) != Inf

proc rnum(p: JsonNode; key: string; default: float32): float32 =
  let v = p.pget(key)
  if v.isNil: return default
  let x = v.asF64
  if x.isNone or not isFin(x.get): raise rbad("`" & key & "` must be a finite number (got " & jsonText(v) & ")")
  float32(x.get)

proc coord(v: JsonNode): Option[float64] =
  let x = v.asF64
  if x.isSome and isFin(x.get): x else: none(float64)

proc pixel(x, y: float64): (int, int) =
  proc cl(v: float64): int = int(floor(clamp(v, float64(low(int32)), float64(high(int32)))))
  (cl(x), cl(y))

proc clicks(p: JsonNode): seq[(int, int)] =
  ## One click, or a batch of clicks, as document pixels.
  let arr = p.pget("points")
  if arr.isArray:
    if arr.len == 0: raise rbad("`points` is empty")
    if arr.len > MaxPoints: raise rbad("at most " & $MaxPoints & " points")
    for q in arr.elems:
      if q.isArray and q.len == 2:
        let (x, y) = (coord(q.elems[0]), coord(q.elems[1]))
        if x.isSome and y.isSome:
          result.add pixel(x.get, y.get)
          continue
      raise rbad("bad point " & jsonText(q) & " (want [x, y])")
    return
  let (xv, yv) = (p.pget("x"), p.pget("y"))
  let (x, y) = ((if xv.isNil: none(float64) else: coord(xv)), (if yv.isNil: none(float64) else: coord(yv)))
  if x.isSome and y.isSome: return @[pixel(x.get, y.get)]
  if x.isNone and y.isNone and xv.isNil and yv.isNil: raise rbad("missing `x`,`y` or `points`")
  raise rbad("`x` and `y` must be finite numbers")

proc readRect(cx, cy: int; radius: float32): Rect =
  let r = (if radius == radius and abs(radius) != Inf: int(clamp(ceil(radius), 0'f32, 256'f32)) else: 64)
  let ext = satAdd(r, int(haloRadius()))
  let area = rect(satSub(cx, ext), satSub(cy, ext), satAdd(satAdd(cx, ext), 1), satAdd(satAdd(cy, ext), 1))
  let (w, h) = (uint64(area.width), uint64(area.height))
  let cap = uint64(MaxWindow)
  if area.isEmpty or w * h > cap * cap: raise rbad("pupil window is larger than " & $MaxWindow & "² px")
  area

proc rgbaRegion(surf: Surface; area: Rect): seq[array[4, float32]] =
  result = newSeq[array[4, float32]](area.width * area.height)
  if not area.isEmpty and result.len > 0: surf.readRgbaInto(area, result)

proc rgbChanged(a, b: array[4, float32]): bool =
  abs(a[0] - b[0]) > 1e-5'f32 or abs(a[1] - b[1]) > 1e-5'f32 or abs(a[2] - b[2]) > 1e-5'f32

proc writeRgba(surf: var Surface; area: Rect; before, px: seq[array[4, float32]]) =
  ## Write back only the pixels whose RGBA changed. Untouched pixels keep their original channel
  ## data, so CMYK (K) and Lab values outside the corrected pupil are never re-encoded.
  if area.isEmpty: return
  let fmt = surf.format
  let n = fmt.channels
  let need = area.width * area.height
  if px.len < need or before.len < need or n == 0: return
  var data = surf.readRegion(area)
  if data.len != need * n: return
  for i in 0 ..< need:
    if not rgbChanged(before[i], px[i]): continue
    let alpha = data[i * n + n - 1]
    discard fromRgbaInto(fmt, px[i], data.toOpenArray(i * n, i * n + n - 1))
    if fmt.alpha: data[i * n + n - 1] = alpha
  surf.writeRegion(area, data)

proc mixSelection(orig: seq[array[4, float32]]; px: var seq[array[4, float32]]; area: Rect; sel: Surface) =
  let w = area.width
  if w == 0: return
  for i in 0 ..< min(px.len, orig.len):
    let (y, x) = (i div w, i mod w)
    let k = clamp(sel.sampleChannel(satAdd(area.x0, x), satAdd(area.y0, y), 0), 0'f32, 1'f32)
    if k >= 1: continue
    let o = orig[i]
    if k <= 0:
      px[i] = o
      continue
    px[i][0] = o[0] + (px[i][0] - o[0]) * k
    px[i][1] = o[1] + (px[i][1] - o[1]) * k
    px[i][2] = o[2] + (px[i][2] - o[2]) * k
    px[i][3] = o[3]

proc restoreLocked(orig: seq[array[4, float32]]; px: var seq[array[4, float32]]) =
  for i in 0 ..< min(px.len, orig.len):
    if orig[i][3] > 1.0e-5'f32: continue
    px[i] = orig[i]

proc countChanged(orig, px: seq[array[4, float32]]): uint32 =
  var n = 0'u64
  for i in 0 ..< min(orig.len, px.len):
    if rgbChanged(orig[i], px[i]): inc n
  uint32(min(n, uint64(high(uint32))))

proc applyClick(surf: var Surface; sel: Option[Surface]; lock: bool; click: (int, int); pupil, darken: float32;
                clip: Rect): (Rect, uint32) =
  let area = readRect(click[0], click[1], searchRadius(pupil)).intersect(clip)
  if area.isEmpty: return (EmptyRect, 0'u32)
  let (w, h) = (area.width, area.height)
  var px = rgbaRegion(surf, area)
  let orig = px
  let n = aredeye.apply(px, w, h, click, (area.x0, area.y0), pupil, darken)
  if n == 0: return (area, 0'u32)
  if sel.isSome: mixSelection(orig, px, area, sel.get)
  if lock: restoreLocked(orig, px)
  let changed = countChanged(orig, px)
  if changed == 0: return (area, 0'u32)
  writeRgba(surf, area, orig, px)
  (area, changed)

proc redEyeE(s: Session): string =
  let why = hasPaintableE(s)
  if why.len > 0: return why
  if s.editsChannel: return ""
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  let id = d.activeLayer.get
  let locks = d.doc[].effectiveLocks(id)
  if locks.pixels or locks.all:
    let l = d.doc[].findLayer(id)
    return "the layer \"" & (if l.isNil: "layer" else: l[].name) & "\" is locked"
  ""

proc redEye(s: var Session; p: JsonNode): JsonNode =
  let pts = clicks(p)
  let pupilRaw = rnum(p, "pupilSize", 50)
  let darken0 = rnum(p, "darken", 50)
  if unclampedWindowSide(pupilRaw).isNone: raise rbad("pupil window is larger than " & $MaxWindow & "² px")
  let pupil = clamp(pupilRaw, 1'f32, 100'f32)
  let darken = clamp(darken0, 0'f32, 100'f32)
  let lp = p.pget("layer").asU64
  let id = (if lp.isSome: some(LayerId(lp.get)) elif s.active.isNil: none(LayerId) else: s.active.activeLayer)
  let (damage, total) = s.editWith("Red Eye", proc(doc: var Document; active: var Option[LayerId]): (Rect, uint32) =
    let canvas = doc.bounds
    let sel = doc.selection
    let (surf, lock) = targetSurface(doc, (if id.isSome: id else: active), p)
    var total = 0'u32
    var damage = EmptyRect
    for click in pts:
      let (area, n) = applyClick(surf[], sel, lock, click, pupil, darken, canvas)
      total = (if high(uint32) - total < n: high(uint32) else: total + n)
      damage = (if damage.isEmpty: area else: damage.union(area))
    if total == 0: raise other(Miss)
    (damage, total))
  let st = s.active
  if not st.isNil: st.lastDamage = some(damage)
  %*{"damage": [damage.x0, damage.y0, damage.width, damage.height], "pixels": total}

proc redEyeSpecs*(): seq[CommandSpec] =
  @[cmd(RedEyeCmd, "Red Eye", [], "",
        """{"x":px, "y":px} | {"points":[[x,y],…] (at most 64)},"pupilSize":1..100=50,"darken":0..100=50,"layer":id?=active,"target":"pixels"|"mask"|"quickMask"|{"channel":i} → {"damage":[x,y,w,h], "pixels":n}""",
        redEyeE, runProc(redEye(s, p)))]
