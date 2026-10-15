## Filter › Lens Correction (and File › Automate › Lens Correction; port of `lens.rs`):
## geometric distortion, chromatic aberration, vignetting and perspective / rotation / scale, with
## Photoshop's edge modes.
##
## * **Distortion**: Brown–Conrady radial terms (D. C. Brown, *Decentering Distortion of Lenses*,
##   1966; A. Conrady 1919): an output point at normalised radius `r` (1 = half the frame
##   diagonal) samples the source at `r·(1 + k1·r² + k2·r⁴)`, plus the tangential `p1, p2` terms.
##   Photoshop's *Remove Distortion* slider adds to `k1`.
## * **Chromatic aberration** (lateral): the red and blue planes are scaled radially about the
##   centre relative to green.
## * **Vignetting**: the profile's `1 + v1·r² + v2·r⁴ + v3·r⁶` falloff is divided out, then the
##   *Vignette Amount / Midpoint* sliders apply an exposure change ramping in from the midpoint.
## * **Transform**: vertical / horizontal perspective as a keystone homography about the frame
##   centre, then the angle and the scale.
##
## The geometry runs as an inverse map per output pixel (bicubic, premultiplied when there is
## alpha), per output tile. A few generic profiles stand in for manufacturer data (which is
## proprietary): `genericProfile` interpolates typical distortion and vignetting over the 35 mm-
## equivalent focal length.

import std/[math, options, json, strutils]
import ../[geom, raster, color]
import ./util

type
  EdgeModeKind* = enum emExtension, emTransparency, emColor
  EdgeMode* = object
    ## What fills the output where the corrected image doesn't reach.
    case kind*: EdgeModeKind
    of emColor: color*: array[4, float32] ## straight RGBA, converted to the document's model
    else: discard

  LensProfile* = object
    ## A lens profile (distortion, vignetting and lateral CA coefficients).
    name*: string
    k1*, k2*, p1*, p2*: float64
    vignette*: array[3, float64] ## falloff `1 + v1 r² + v2 r⁴ + v3 r⁶`
    caRed*, caBlue*: float64      ## radial scale of red / blue relative to green (`1 + ca`)

  LensCorrection* = object
    ## Lens Correction parameters in Photoshop's dialog units.
    profile*: Option[LensProfile] ## applied first (Auto Correction tab)
    correctDistortion*, correctVignette*, correctCa*: bool
    distortion*: float64       ## Remove Distortion −100..=100
    redCyan*: float64          ## Fix Red/Cyan Fringe −100..=100
    blueYellow*: float64       ## Fix Blue/Yellow Fringe −100..=100
    vignetteAmount*: float64   ## −100..=100 (positive lightens the corners)
    vignetteMidpoint*: float64 ## 0..=100
    vertical*, horizontal*: float64 ## perspective −100..=100
    angle*: float64            ## degrees (positive turns the image clockwise)
    scale*: float64            ## percent (50..=150)
    edge*: EdgeMode

  Sample* = object
    ## Where an output pixel samples the source: green (and alpha / non-RGB channels) at `p`, red
    ## at `pr`, blue at `pb`, then the colour is multiplied by `gain`.
    p*, pr*, pb*: array[2, float64]
    gain*: float32

  LensMap* = object
    ## The inverse map of a `LensCorrection` over a frame.
    c: array[2, float64]
    r: float64 ## half diagonal (radius normaliser)
    k1, k2, p1, p2, caR, caB: float64
    vigProfile: array[3, float64]
    vigAmount, vigMid: float64
    hinv: array[9, float64] ## inverse keystone (normalised coords)
    rot: (float64, float64)
    scale: float64

  SampleMap* = proc(x, y: float64): Option[Sample]

proc `==`*(a, b: EdgeMode): bool =
  a.kind == b.kind and (a.kind != emColor or a.color == b.color)

proc edgeTransparency*(): EdgeMode = EdgeMode(kind: emTransparency)
proc edgeExtension*(): EdgeMode = EdgeMode(kind: emExtension)
proc edgeColor*(c: array[4, float32]): EdgeMode = EdgeMode(kind: emColor, color: c)

proc defaultLensCorrection*(): LensCorrection =
  LensCorrection(correctDistortion: true, correctVignette: true, correctCa: true, vignetteMidpoint: 50, scale: 100,
                 edge: edgeTransparency())

proc rustFixed0(v: float64): string =
  ## Rust's `{:.0}` for f64.
  if v != v: return "NaN"
  if v == Inf: return "inf"
  if v == NegInf: return "-inf"
  result = formatFloat(v, ffDecimal, 0)
  if result.endsWith("."): result.setLen(result.len - 1)

proc genericProfile*(focal35mm: float64): LensProfile =
  ## Generic profile for a 35 mm-equivalent focal length (typical zoom-lens behaviour: barrel
  ## distortion and strong vignetting at the wide end, mild pincushion at the long end).
  const Table = [(12.0, -0.16, 0.04, -0.60, 0.15), (16.0, -0.11, 0.025, -0.50, 0.12), (24.0, -0.06, 0.012, -0.38, 0.08),
                 (35.0, -0.025, 0.004, -0.28, 0.05), (50.0, 0.0, 0.0, -0.22, 0.04), (85.0, 0.012, 0.0, -0.16, 0.02),
                 (200.0, 0.02, 0.0, -0.10, 0.01)]
  let f = clamp(focal35mm, Table[0][0], Table[6][0])
  var k = 5
  for i in 1 ..< Table.len:
    if f <= Table[i][0]:
      k = i - 1
      break
  let (a, b) = (Table[k], Table[k + 1])
  # Interpolate on log focal length.
  let t = (ln(f) - ln(a[0])) / (ln(b[0]) - ln(a[0]))
  proc l(x, y: float64): float64 = x + (y - x) * t
  LensProfile(name: "Generic Lens (" & rustFixed0(focal35mm) & " mm equivalent)", k1: l(a[1], b[1]), k2: l(a[2], b[2]),
              vignette: [l(a[3], b[3]), l(a[4], b[4]), 0.0])

proc newLensMap*(lc: LensCorrection; frame: Rect): LensMap =
  let c = [float64(frame.x0 + frame.x1) / 2.0, float64(frame.y0 + frame.y1) / 2.0]
  let r = rmax(hypot(float64(frame.width), float64(frame.height)), 2.0) / 2.0
  let prof = lc.profile.get(LensProfile())
  let (pk1, pk2, pp1, pp2) = (if lc.correctDistortion: (prof.k1, prof.k2, prof.p1, prof.p2) else: (0.0, 0.0, 0.0, 0.0))
  # Keystone x' = x / (1 + h x + g y), y' = y / (1 + h x + g y) (normalised by r).
  let g = -lc.vertical / 100.0 * 0.5
  let h = -lc.horizontal / 100.0 * 0.5
  # Inverse of [[1,0,0],[0,1,0],[h,g,1]] is [[1,0,0],[0,1,0],[-h,-g,1]].
  let hinv = [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, -h, -g, 1.0]
  let a = toRadians(lc.angle)
  LensMap(c: c, r: r, k1: pk1 - lc.distortion / 100.0 * 0.25, k2: pk2, p1: pp1, p2: pp2,
          caR: (if lc.correctCa: prof.caRed else: 0.0) - lc.redCyan * 4e-5,
          caB: (if lc.correctCa: prof.caBlue else: 0.0) - lc.blueYellow * 4e-5,
          vigProfile: (if lc.correctVignette: prof.vignette else: [0.0, 0.0, 0.0]),
          vigAmount: lc.vignetteAmount / 100.0, vigMid: clamp(lc.vignetteMidpoint, 0.0, 100.0) / 100.0,
          hinv: hinv, rot: (sin(a), cos(a)), scale: clamp(lc.scale / 100.0, 0.05, 20.0))

proc sample*(m: LensMap; x, y: float64): Option[Sample] =
  ## Output px → where to sample (none when the keystone sends it to infinity).
  # Undo scale and rotation (about the centre).
  let (dx, dy) = ((x - m.c[0]) / m.scale, (y - m.c[1]) / m.scale)
  let (s, co) = m.rot
  let (rx, ry) = (co * dx + s * dy, -s * dx + co * dy)
  # Undo the keystone.
  let (nx, ny) = (rx / m.r, ry / m.r)
  let w = m.hinv[6] * nx + m.hinv[7] * ny + m.hinv[8]
  if w <= 0.05: return none(Sample)
  let (ux, uy) = (nx / w, ny / w)
  # Lens distortion (Brown–Conrady).
  let r2 = ux * ux + uy * uy
  let radial = 1.0 + m.k1 * r2 + m.k2 * r2 * r2
  let sx = ux * radial + 2.0 * m.p1 * ux * uy + m.p2 * (r2 + 2.0 * ux * ux)
  let sy = uy * radial + m.p1 * (r2 + 2.0 * uy * uy) + 2.0 * m.p2 * ux * uy
  proc toPx(k: float64): array[2, float64] = [m.c[0] + sx * k * m.r, m.c[1] + sy * k * m.r]
  # Vignetting on the source radius.
  let rs2 = sx * sx + sy * sy
  let (v1, v2, v3) = (m.vigProfile[0], m.vigProfile[1], m.vigProfile[2])
  let falloff = rmax(1.0 + v1 * rs2 + v2 * rs2 * rs2 + v3 * rs2 * rs2 * rs2, 0.05)
  let start = (1.0 - m.vigMid) * 0.9
  let t = clamp((sqrt(rs2) - start) / (1.0 - start + 1e-9), 0.0, 1.0)
  let ramp = t * t * (3.0 - 2.0 * t)
  let gain = pow(2.0, m.vigAmount * 1.5 * ramp) / falloff
  some(Sample(p: toPx(1.0), pr: toPx(1.0 + m.caR), pb: toPx(1.0 + m.caB), gain: float32(gain)))

proc autoScale*(lc: LensCorrection; frame: Rect): float64 =
  ## Smallest scale (percent) at which the corrected image covers the whole frame (Auto Scale).
  proc covers(scale: float64): bool =
    var l = lc
    l.scale = scale
    let m = newLensMap(l, frame)
    let (x0, y0, x1, y1) = (float64(frame.x0), float64(frame.y0), float64(frame.x1), float64(frame.y1))
    for i in 0 .. 40:
      let t = float64(i) / 40.0
      for (x, y) in [(x0 + t * (x1 - x0), y0), (x0 + t * (x1 - x0), y1), (x0, y0 + t * (y1 - y0)), (x1, y0 + t * (y1 - y0))]:
        let s = m.sample(x, y)
        if not (s.isSome and s.get.p[0] >= x0 - 0.5 and s.get.p[0] <= x1 + 0.5 and s.get.p[1] >= y0 - 0.5 and s.get.p[1] <= y1 + 0.5):
          return false
    true
  if covers(lc.scale): return lc.scale
  var (lo, hi) = (lc.scale, lc.scale)
  # Rust loops forever here on a zero or negative scale (`hi` never grows); start such searches
  # from 1 % instead.
  if hi <= 0.0: (lo, hi) = (1.0, 1.0)
  while not covers(hi) and hi < 400.0: hi *= 1.25
  for _ in 0 ..< 30:
    let mid = (lo + hi) / 2.0
    if covers(mid): hi = mid else: lo = mid
  hi

proc straightenAngle*(a, b: array[2, float64]): float64 =
  ## Angle (degrees, for `LensCorrection.angle`) that makes the line `a → b` horizontal or
  ## vertical, whichever it is closer to (the Straighten tool).
  let deg = toDegrees(arctan2(b[1] - a[1], b[0] - a[0]))
  # Nearest multiple of 90°.
  let target = round(deg / 90.0) * 90.0
  -(deg - target)

const Tile = 128

proc remap*(src: Surface; frame, outArea: Rect; edge: EdgeMode; map: SampleMap): Surface =
  ## Resamples `src` through an inverse map over `outArea`. `frame` is the image the map refers
  ## to: samples clamp to it, and pixels mapping outside it take the edge mode. `map` returns where
  ## each output pixel centre samples (none = outside).
  let sfmt = src.format
  let needsAlpha = edge.kind == emTransparency and not sfmt.alpha
  let ofmt = (if needsAlpha: pixelFormat(sfmt.mode, sfmt.sample, true) else: sfmt)
  result = newSurface(ofmt)
  if outArea.isEmpty or frame.isEmpty: return
  let nIn = sfmt.channels
  let nOut = ofmt.channels
  let cc = sfmt.mode.colorChannels
  let rgb = sfmt.mode in {cmRgb, cmIndexed} and cc == 3
  let subtractive = sfmt.mode == cmCmyk
  let lab = sfmt.mode == cmLab
  let edgePx = (if edge.kind == emColor: fromRgba(ofmt, edge.color) else: newSeq[float32](nOut))
  let (fx0, fy0, fx1, fy1) = (float64(frame.x0), float64(frame.y0), float64(frame.x1), float64(frame.y1))
  var ty = outArea.y0
  while ty < outArea.y1:
    var tx = outArea.x0
    while tx < outArea.x1:
      let t = rect(tx, ty, min(tx + Tile, outArea.x1), min(ty + Tile, outArea.y1))
      tx += Tile
      let (tw, th) = (t.width, t.height)
      var samples = newSeq[Option[Sample]](tw * th)
      for i in 0 ..< tw * th: samples[i] = map(float64(t.x0) + float64(i mod tw) + 0.5, float64(t.y0) + float64(i div tw) + 0.5)
      # Source window: bbox of every sample, clamped to the frame.
      var (bx0, by0, bx1, by1) = (F64Max, F64Max, -F64Max, -F64Max)
      for s in samples:
        if s.isNone: continue
        for p in [s.get.p, s.get.pr, s.get.pb]:
          bx0 = rmin(bx0, p[0])
          by0 = rmin(by0, p[1])
          bx1 = rmax(bx1, p[0])
          by1 = rmax(by1, p[1])
      var data = newSeq[float32](tw * th * nOut)
      var win = EmptyRect
      if not (bx0 > bx1):
        proc clx(v: float64): int = clamp(asI32(v), frame.x0, frame.x1)
        proc cly(v: float64): int = clamp(asI32(v), frame.y0, frame.y1)
        win = rect(clx(floor(bx0) - 3.0), cly(floor(by0) - 3.0), clx(ceil(bx1) + 4.0), cly(ceil(by1) + 4.0))
      let (ww, wh) = (win.width, win.height)
      var buf = (if win.isEmpty: newSeq[float32]() else: src.readRegion(win))
      # Premultiply colour by alpha so transparent pixels don't bleed.
      if sfmt.alpha:
        for q in 0 ..< buf.len div nIn:
          let a = buf[q * nIn + nIn - 1]
          for c in 0 ..< nIn - 1: buf[q * nIn + c] *= a
      template bicubic(ch: int; p: array[2, float64]): float32 =
        # Pixel centres at +0.5; clamp to the frame (edge extension).
        block:
          let x = clamp(p[0] - 0.5, fx0, fx1 - 1.0)
          let y = clamp(p[1] - 0.5, fy0, fy1 - 1.0)
          let (xi, yi) = (floor(x), floor(y))
          let (wx, wy) = (catmullRom(float32(x - xi)), catmullRom(float32(y - yi)))
          var acc = 0'f32
          for j in 0 .. 3:
            let yy = clamp(asI32(yi) + j - 1, win.y0, win.y1 - 1) - win.y0
            for i in 0 .. 3:
              let xx = clamp(asI32(xi) + i - 1, win.x0, win.x1 - 1) - win.x0
              acc += wx[i] * wy[j] * buf[(yy * ww + xx) * nIn + ch]
          acc
      for i, so in samples:
        let o = i * nOut
        if so.isNone or ww <= 0 or wh <= 0:
          for c in 0 ..< nOut: data[o + c] = edgePx[c]
          continue
        let s = so.get
        # Inside-frame weight (1 px soft edge) unless edges extend.
        var inside = 1'f32
        if edge.kind != emExtension:
          let fx = clamp(rmin(s.p[0] - fx0, fx1 - s.p[0]) + 0.5, 0.0, 1.0)
          let fy = clamp(rmin(s.p[1] - fy0, fy1 - s.p[1]) + 0.5, 0.0, 1.0)
          inside = float32(fx * fy)
        let alpha = (if sfmt.alpha: clamp(bicubic(nIn - 1, s.p), 0'f32, 1'f32) else: 1'f32)
        var col: array[8, float32]
        for c in 0 ..< cc:
          let p = (if rgb and c == 0: s.pr elif rgb and c == 2: s.pb else: s.p)
          let pm = bicubic(c, p)
          let straight = (if sfmt.alpha: (if alpha > 1e-6'f32: pm / alpha else: 0'f32) else: pm)
          let g = s.gain
          col[c] = (if subtractive: 1'f32 - (1'f32 - straight) * g
                    elif lab: (if c == 0: straight * g else: straight)
                    else: straight * g)
          if sfmt.sample != stF32: col[c] = clamp(col[c], 0'f32, 1'f32)
        let aOut = alpha * inside
        if edge.kind == emColor and inside < 1'f32:
          # Blend towards the edge colour by the outside share.
          let ea = (if ofmt.alpha: edgePx[nOut - 1] else: 1'f32)
          let oa = alpha * inside + ea * (1'f32 - inside)
          for c in 0 ..< cc:
            let mixed = col[c] * alpha * inside + edgePx[c] * ea * (1'f32 - inside)
            data[o + c] = (if oa > 1e-6'f32: mixed / oa else: 0'f32)
          if ofmt.alpha: data[o + nOut - 1] = oa
        else:
          for c in 0 ..< cc: data[o + c] = col[c]
          if ofmt.alpha: data[o + nOut - 1] = aOut
      result.writeRegion(t, data)
    ty += Tile
  result.prune()

proc correct*(src: Surface; frame: Rect; lc: LensCorrection): Surface =
  ## Applies a lens correction to `src` over `frame` (normally the canvas).
  let m = newLensMap(lc, frame)
  remap(src, frame, frame, lc.edge, proc(x, y: float64): Option[Sample] = m.sample(x, y))

# ---------------------------------------------------------------- JSON options

proc num(n: JsonNode; name: string): float64 =
  case n.kind
  of JInt: float64(n.getBiggestInt)
  of JFloat: n.getFloat
  else: raise newException(ValueError, "invalid type for " & name & ": expected a number")

proc boolean(n: JsonNode; name: string): bool =
  if n.kind != JBool: raise newException(ValueError, "invalid type for " & name & ": expected a boolean")
  n.getBool

proc edgeModeFromJson*(j: JsonNode): EdgeMode =
  ## `"extension"`, `"transparency"` or `{"color": [r, g, b, a]}`.
  if j.kind == JString:
    case j.getStr
    of "extension": return edgeExtension()
    of "transparency": return edgeTransparency()
    else: discard
  elif j.kind == JObject and j.len == 1 and j.hasKey("color") and j["color"].kind == JArray and j["color"].len == 4:
    var c: array[4, float32]
    for i in 0 .. 3: c[i] = float32(num(j["color"][i], "color"))
    return edgeColor(c)
  raise newException(ValueError, "unknown edge mode " & $j)

proc lensProfileFromJson*(j: JsonNode): LensProfile =
  ## Every field is required (as in the Rust build).
  if j.kind != JObject: raise newException(ValueError, "a lens profile is an object")
  for k in ["name", "k1", "k2", "p1", "p2", "vignette", "ca_red", "ca_blue"]:
    if not j.hasKey(k): raise newException(ValueError, "missing field `" & k & "`")
  if j["name"].kind != JString: raise newException(ValueError, "invalid type for name")
  if j["vignette"].kind != JArray or j["vignette"].len != 3: raise newException(ValueError, "vignette is [v1, v2, v3]")
  LensProfile(name: j["name"].getStr, k1: num(j["k1"], "k1"), k2: num(j["k2"], "k2"), p1: num(j["p1"], "p1"),
              p2: num(j["p2"], "p2"),
              vignette: [num(j["vignette"][0], "vignette"), num(j["vignette"][1], "vignette"), num(j["vignette"][2], "vignette")],
              caRed: num(j["ca_red"], "ca_red"), caBlue: num(j["ca_blue"], "ca_blue"))

proc lensCorrectionFromJson*(j: JsonNode): LensCorrection =
  ## The options from their JSON form (camelCase, missing fields take their defaults); raises
  ## `ValueError` on a malformed value.
  if j.kind != JObject: raise newException(ValueError, "Lens Correction options must be an object")
  result = defaultLensCorrection()
  for k, v in j:
    case k
    of "profile": result.profile = (if v.kind == JNull: none(LensProfile) else: some(lensProfileFromJson(v)))
    of "correctDistortion": result.correctDistortion = boolean(v, k)
    of "correctVignette": result.correctVignette = boolean(v, k)
    of "correctCa": result.correctCa = boolean(v, k)
    of "distortion": result.distortion = num(v, k)
    of "redCyan": result.redCyan = num(v, k)
    of "blueYellow": result.blueYellow = num(v, k)
    of "vignetteAmount": result.vignetteAmount = num(v, k)
    of "vignetteMidpoint": result.vignetteMidpoint = num(v, k)
    of "vertical": result.vertical = num(v, k)
    of "horizontal": result.horizontal = num(v, k)
    of "angle": result.angle = num(v, k)
    of "scale": result.scale = num(v, k)
    of "edge": result.edge = edgeModeFromJson(v)
    else: discard
