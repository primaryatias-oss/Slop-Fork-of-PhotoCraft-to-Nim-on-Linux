## Mixer Brush (port of `paint/src/mixer.rs`): a reservoir of loaded paint plus a pickup well that
## mixes in canvas colour.
##
## Model (behavioural, our own): each dab samples the canvas colour under its footprint. The pickup
## colour `P` moves towards it by `wet`. The deposited colour mixes the reservoir `R` (weight
## `amount × (1 − mix·wet)`) with the pickup (weight `mix·wet`); it lands at `flow` times the total
## weight. The reservoir amount drains per dab unless `load` is 100 %, so low Load strokes dry out.
## A dry brush (`wet = 0`) paints only reservoir colour; an empty, wet brush smears canvas colour.

import std/[options, math]
import ../geom, ../color, ../raster, ../rmath, ../serde
import ./brush, ./retouch

const PickupRate* = 0.25'f32
  ## Fraction of the way the pickup colour moves towards the canvas colour per dab at 100 % Wet.

type
  MixerState* = object
    ## Paint carried between strokes (session tool state).
    reservoir*: Option[array[4, float32]] ## loaded paint (straight RGBA), none = clean/unloaded
    amount*: float32                      ## remaining reservoir amount 0..1
    pickup*: Option[array[4, float32]]    ## colour picked up from the canvas, none = clean

proc serdeCamel*(_: typedesc[MixerState]): bool = true

proc load*(s: var MixerState; color: array[4, float32]) =
  ## Load the reservoir with a colour (full).
  s.reservoir = some(color)
  s.amount = 1

proc clean*(s: var MixerState) =
  ## Clean the brush (drop the picked-up colour).
  s.pickup = none(array[4, float32])

proc applyMixerStroke*(target: var Surface; sample: Option[Surface]; stroke: Stroke; m: MixerSettings;
                       state: var MixerState; selection: Option[Surface]; lockTransparency: bool): Rect =
  ## Paint a Mixer Brush stroke. `sample` is the surface colour is picked up from (the layer itself
  ## when none; a flattened composite for Sample All Layers). Returns the damaged rectangle.
  let fmt = target.format
  let n = fmt.channels
  let a = alphaIndex(fmt)
  let nc = fmt.mode.colorChannels
  proc native(c: array[4, float32]): seq[float32] =
    result = fromRgba(fmt, c)
    if a >= 0: result[a] = 1
  let hasRes = state.reservoir.isSome
  let res = if hasRes: native(state.reservoir.get) else: newSeq[float32](n)
  var amount = if hasRes: clamp(state.amount, 0'f32, 1'f32) else: 0'f32
  var hasPickup = state.pickup.isSome
  var pickup = if hasPickup: native(state.pickup.get) else: newSeq[float32](n)
  let (wet0, mix0, flow, load) = (clamp(m.wet, 0'f32, 1'f32), clamp(m.mix, 0'f32, 1'f32),
                                  clamp(m.flow, 0'f32, 1'f32), clamp(m.load, 0'f32, 1'f32))
  var st = stroke
  st.brush.opacity = 1
  var acc = newSeq[float32](n)
  var paint = newSeq[float32](n)
  let dmg = applyDabStroke(target, st, selection, lockTransparency, 0, proc(work: var PxRegion; fp: Footprint) =
    # Transfer › Wetness / Mix Jitter scale the settings per dab.
    let (wet, mix) = (wet0 * clamp(fp.dab.wet, 0'f32, 1'f32), mix0 * clamp(fp.dab.mix, 0'f32, 1'f32))
    # Sample the canvas under the dab.
    for v in acc.mitems: v = 0
    var (wsum, asum) = (0'f32, 0'f32)
    let r = fp.rect.intersect(work.rect)
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        let w = fp.at(x, y)
        if w <= 0: continue
        let px =
          if sample.isSome:
            let s = sample.get
            if s.format == fmt: s.pixel(x, y) else: fromRgba(fmt, s.rgba(x, y))
          else: work.pxSeq(x, y)
        let al = if a >= 0: px[a] else: 1'f32
        for c in 0 ..< nc: acc[c] += px[c] * w * al
        wsum += w
        asum += w * al
    if asum > 1e-6'f32 and wet > 0:
      let ca = asum / rmax(wsum, 1e-6'f32)
      if not hasPickup:
        for c in 0 ..< n: pickup[c] = (if c < nc: acc[c] / asum else: 1'f32)
        hasPickup = true
      else:
        for c in 0 ..< n:
          let cv = if c < nc: acc[c] / asum else: 1'f32
          pickup[c] = pickup[c] + (cv - pickup[c]) * PickupRate * wet * ca
    let wr = if hasRes: amount * (1'f32 - mix * wet) else: 0'f32
    let wp = if hasPickup: mix * wet else: 0'f32
    let total = wr + wp
    if total > 1e-6'f32:
      for c in 0 ..< n:
        let rv = if hasRes: res[c] else: 0'f32
        let pv = if hasPickup: pickup[c] else: 0'f32
        paint[c] = (rv * wr + pv * wp) / total
      let dep = rmin(total, 1'f32) * flow
      for y in r.y0 ..< r.y1:
        for x in r.x0 ..< r.x1:
          let k = fp.at(x, y) * dep
          if k > 0:
            let o = work.offset(x, y)
            overNative(fmt, work.data.toOpenArray(o, o + n - 1), paint, k, lockTransparency)
    amount *= 1'f32 - 0.08'f32 * (1'f32 - load))
  state.amount = amount
  state.pickup = if hasPickup: some(toRgba(fmt, pickup)) else: none(array[4, float32])
  dmg
