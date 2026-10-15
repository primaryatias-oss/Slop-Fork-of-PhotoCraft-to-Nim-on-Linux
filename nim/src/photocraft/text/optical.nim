## Optical kerning (Photoshop's Character panel › Kerning › Optical): pair spacing from the glyph
## outlines instead of the font's kerning table (port of `photocraft_text::optical`).
##
## Each glyph gets a horizontal profile: at `Samples` heights across the em, the distance from the
## start of its advance to its leftmost ink and from its rightmost ink to the end of its advance
## (em units). A pair's closest approach and mean white space beyond it, compared with the font's
## own "oo" pair, give the kerning through coefficients fitted to Photoshop's output.

import std/[tables, options, math, hashes]
import ../font/sfnt

const
  Samples* = 48
  YLo = -0.25'f32
  YHi = 1.0'f32
  BandLo = -0.1'f32
  BandHi = 1.0'f32
  Depth = 0.04'f32
  Near = 0.1'f32
  C0 = -0.0033'f32
  CMin = 0.5384'f32
  CExcess = 0.5526'f32
  CNear = -0.1612'f32
  Limit = 400'f32
  MaxProfiles = 50_000

type
  Profile* = object
    left*, right*: array[Samples, Option[float32]]
    advance*: float32

  OpticalCache* = object
    profiles: Table[(Hash, int), Option[Profile]]
    reference: Table[Hash, Option[(float32, float32)]]

proc isEmpty*(p: Profile): bool =
  for x in p.left:
    if x.isSome: return false
  true

proc sampleY*(i: int): float32 = YLo + (YHi - YLo) * (float32(i) + 0.5'f32) / float32(Samples)

proc flatten(o: Outline): seq[array[4, float32]] =
  var start, at: (float32, float32)
  template line(x, y: float32) =
    result.add [at[0], at[1], x, y]
    at = (x, y)
  for c in o:
    case c.verb
    of pvMove:
      start = c.pts[^1]
      at = start
    of pvLine: line(c.pts[^1][0], c.pts[^1][1])
    of pvQuad:
      let (x0, y0) = at
      let (cx, cy) = c.pts[0]
      let (x, y) = c.pts[^1]
      for i in 1 .. 8:
        let t = float32(i) / 8
        let u = 1 - t
        line(u * u * x0 + 2 * u * t * cx + t * t * x, u * u * y0 + 2 * u * t * cy + t * t * y)
    of pvCubic:
      let (x0, y0) = at
      let (c0x, c0y) = c.pts[0]
      let (c1x, c1y) = c.pts[1]
      let (x, y) = c.pts[^1]
      for i in 1 .. 12:
        let t = float32(i) / 12
        let u = 1 - t
        let (a, b, cc, d) = (u * u * u, 3 * u * u * t, 3 * u * t * t, t * t * t)
        line(a * x0 + b * c0x + cc * c1x + d * x, a * y0 + b * c0y + cc * c1y + d * y)
    of pvClose:
      if start != at: line(start[0], start[1])

proc profile*(f: Face; gid: int): Option[Profile] =
  ## The profile of glyph `gid` (none if the glyph can't be read).
  let upem = float32(f.unitsPerEm)
  if upem.isNaN or upem <= 0: return
  var p = Profile(advance: f.advance(gid) / upem)
  var segs: seq[array[4, float32]]
  try: segs = flatten(f.outline(gid))
  except FontError: return
  for i in 0 ..< Samples:
    let y = sampleY(i) * upem
    var lo = Inf.float32
    var hi = NegInf.float32
    for s in segs:
      let (x0, y0, x1, y1) = (s[0], s[1], s[2], s[3])
      if (y0 <= y and y < y1) or (y1 <= y and y < y0):
        let x = x0 + (x1 - x0) * (y - y0) / (y1 - y0)
        lo = min(lo, x)
        hi = max(hi, x)
    if lo.classify notin {fcInf, fcNegInf, fcNan} and hi.classify notin {fcInf, fcNegInf, fcNan}:
      p.left[i] = some(lo / upem)
      p.right[i] = some(p.advance - hi / upem)
  some(p)

proc pairFeatures*(l, r: Profile): Option[(float32, float32)] =
  ## Closest approach and mean white space beyond it (capped at `Depth`), em.
  var gaps: seq[float32]
  for i in 0 ..< Samples:
    let y = sampleY(i)
    if y < BandLo or y > BandHi: continue
    if l.right[i].isSome and r.left[i].isSome: gaps.add l.right[i].get + r.left[i].get
  if gaps.len == 0: return
  var mn = Inf.float32
  for g in gaps: mn = min(mn, g)
  var excess = 0'f32
  for g in gaps: excess += min(g - mn, Depth)
  some((mn, excess / float32(gaps.len)))

proc sizeAdjust*(sizePt: float32): float32 =
  ## Photoshop opens pairs at small sizes by about 11 × ln(95 / size) thousandths of an em.
  if sizePt.isNaN or sizePt.classify in {fcInf, fcNegInf} or sizePt <= 0: return 0
  11 * ln(95 / clamp(sizePt, 15'f32, 95'f32))

proc kernFromFeatures*(f, reference: (float32, float32)): float32 =
  var k = C0 + CMin * (reference[0] - f[0]) + CExcess * (reference[1] - f[1]) +
          CNear * (min(reference[0], Near) - min(f[0], Near))
  k *= 1000
  if k.isNaN or k.classify in {fcInf, fcNegInf}: 0 else: clamp(k, -Limit, Limit)

proc fontKey(fontId: uint64; index: int; coords: openArray[int16]): Hash =
  var h: Hash = 0
  h = h !& hash(fontId)
  h = h !& hash(index)
  for c in coords: h = h !& hash(c)
  !$h

proc get(c: var OpticalCache; fk: Hash; f: Face; gid: int): Option[Profile] =
  if c.profiles.len > MaxProfiles: c.profiles.clear()
  let key = (fk, gid)
  if key notin c.profiles: c.profiles[key] = profile(f, gid)
  c.profiles[key]

proc referenceOf(c: var OpticalCache; fk: Hash; f: Face): Option[(float32, float32)] =
  if fk in c.reference: return c.reference[fk]
  var r = none((float32, float32))
  for ch in ['o', 'O', 'n', 'H']:
    let g = (try: f.glyphIndex(ord(ch)) except FontError: 0)
    if g == 0: continue
    let p = c.get(fk, f, g)
    if p.isNone: continue
    let feats = pairFeatures(p.get, p.get)
    if feats.isSome:
      r = feats
      break
  c.reference[fk] = r
  r

proc pair*(c: var OpticalCache; fontId: uint64; f: Face; index: int; coords: openArray[int16];
           left, right: int): Option[float32] =
  ## Optical kerning of a glyph pair of one face in 1/1000 em; none when it doesn't apply.
  let fk = fontKey(fontId, index, coords)
  let l = c.get(fk, f, left)
  if l.isNone: return
  let r = c.get(fk, f, right)
  if r.isNone: return
  let feats = pairFeatures(l.get, r.get)
  if feats.isNone: return
  let refr = c.referenceOf(fk, f)
  if refr.isNone: return
  some(kernFromFeatures(feats.get, refr.get))
