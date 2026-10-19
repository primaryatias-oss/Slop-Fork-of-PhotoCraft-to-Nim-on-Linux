## DNG opcode lists (port of `raw/src/opcodes.rs`; DNG 1.7, chapter 7 "Opcode List Processing").
##
## An opcode list is big-endian whatever the file's byte order: a count, then per opcode its ID,
## DNG version, flags (bit 0 = optional), parameter size and parameters. Only GainMap (ID 9), the
## lens-shading correction phones write in OpcodeList2, is applied; other opcodes are reported.

import std/[options, math]
import ../rmath, ../geom

type
  GainMap* = object
    ## One GainMap opcode: a grid of gains, bilinearly interpolated, applied to the pixels of an
    ## area (in active-area coordinates) at a row / column pitch.
    top*, left*, bottom*, right*: int
    plane*, planes*: int
    rowPitch*, colPitch*: int
    pointsV*, pointsH*: int
    spacingV*, spacingH*, originV*, originH*: float64 ## image-relative (0..1)
    mapPlanes*: int
    gains*: seq[float32] ## `pointsV × pointsH × mapPlanes` gains

proc gainAt(m: GainMap; i: int): float32 {.inline.} =
  if i >= 0 and i < m.gains.len: m.gains[i] else: 1'f32

proc row*(m: GainMap; rv: float64; mp0: int): seq[float32] =
  ## Gains along the map's horizontal points for image-relative row `rv` and map plane `mp`
  ## (vertical interpolation done).
  let mp = min(mp0, max(m.mapPlanes - 1, 0))
  let fv = clamp((rv - m.originV) / m.spacingV, 0.0, float64(m.pointsV - 1))
  let i0 = min(asUsize(floor(fv)), m.pointsV - 1)
  let i1 = min(i0 + 1, m.pointsV - 1)
  let t = float32(fv - float64(i0))
  result = newSeq[float32](m.pointsH)
  for j in 0 ..< m.pointsH:
    let a = m.gainAt((i0 * m.pointsH + j) * m.mapPlanes + mp)
    let b = m.gainAt((i1 * m.pointsH + j) * m.mapPlanes + mp)
    result[j] = a + (b - a) * t

proc at*(m: GainMap; row: openArray[float32]; rh: float64): float32 {.inline.} =
  ## Interpolates a `row` at image-relative column `rh`.
  let fh = clamp((rh - m.originH) / m.spacingH, 0.0, float64(m.pointsH - 1))
  let j0 = min(asUsize(floor(fh)), m.pointsH - 1)
  let j1 = min(j0 + 1, m.pointsH - 1)
  let t = float32(fh - float64(j0))
  let a = if j0 < row.len: row[j0] else: 1'f32
  let b = if j1 < row.len: row[j1] else: 1'f32
  a + (b - a) * t

proc covers*(m: GainMap; ax, ay: int): bool {.inline.} =
  ## Whether the map covers active-area pixel (`ax`, `ay`).
  ay >= m.top and ay < m.bottom and ax >= m.left and ax < m.right and
    (ay - m.top) mod m.rowPitch == 0 and (ax - m.left) mod m.colPitch == 0

proc opcodeName(id: uint32): string =
  ## Opcode names (DNG 1.7 table) for warnings.
  case id
  of 1: "WarpRectilinear"
  of 2: "WarpFisheye"
  of 3: "FixVignetteRadial"
  of 4: "FixBadPixelsConstant"
  of 5: "FixBadPixelsList"
  of 6: "TrimBounds"
  of 7: "MapTable"
  of 8: "MapPolynomial"
  of 9: "GainMap"
  of 10: "DeltaPerRow"
  of 11: "DeltaPerColumn"
  of 12: "ScalePerRow"
  of 13: "ScalePerColumn"
  of 14: "WarpRectilinear2"
  else: "opcode " & $id

proc be32(b: openArray[byte]; at: int): Option[uint32] =
  if at < 0 or at > b.len - 4: return
  some((uint32(b[at]) shl 24) or (uint32(b[at + 1]) shl 16) or (uint32(b[at + 2]) shl 8) or uint32(b[at + 3]))

proc be64f(b: openArray[byte]; at: int): Option[float64] =
  if at < 0 or at > b.len - 8: return
  var v = 0'u64
  for k in 0 .. 7: v = (v shl 8) or uint64(b[at + k])
  let f = cast[float64](v)
  if f == f and f != Inf and f != NegInf: result = some(f)

proc gainMap(p: openArray[byte]): Option[GainMap] =
  var u: array[19, int]
  for i in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 18]:
    let v = be32(p, 4 * i)
    if v.isNone: return
    u[i] = int(v.get)
  let (sv, sh, ov, oh) = (be64f(p, 40), be64f(p, 48), be64f(p, 56), be64f(p, 64))
  if sv.isNone or sh.isNone or ov.isNone or oh.isNone: return
  let (pointsV, pointsH, mapPlanes) = (u[8], u[9], u[18])
  # The product saturates far above the 2^20 cap, so it never wraps.
  let n = if pointsV > 0 and pointsH > 0 and mapPlanes > 0 and pointsV <= (1 shl 21) and pointsH <= (1 shl 21) and mapPlanes <= (1 shl 21):
            pointsV * pointsH * mapPlanes else: (if pointsV == 0 or pointsH == 0 or mapPlanes == 0: 0 else: high(int))
  if n == 0 or n > (1 shl 20) or u[6] == 0 or u[7] == 0 or sv.get <= 0 or sh.get <= 0 or u[2] <= u[0] or u[3] <= u[1]: return
  var gains = newSeq[float32](n)
  for i in 0 ..< n:
    let g = be32(p, 76 + 4 * i)
    if g.isNone: return
    let f = cast[float32](g.get)
    if f != f or f == Inf or f == NegInf: return
    gains[i] = f
  some(GainMap(top: u[0], left: u[1], bottom: u[2], right: u[3], plane: u[4], planes: u[5], rowPitch: u[6],
               colPitch: u[7], pointsV: pointsV, pointsH: pointsH, spacingV: sv.get, spacingH: sh.get,
               originV: ov.get, originH: oh.get, mapPlanes: mapPlanes, gains: gains))

proc parseOpcodes*(list: openArray[byte]): (seq[GainMap], seq[string]) =
  ## Parses an opcode list: the gain maps, and the names of opcodes not applied.
  let count = be32(list, 0)
  if count.isNone: return
  var at = 4
  for _ in 0'u32 ..< min(count.get, 4096'u32):
    let (id, size) = (be32(list, at), be32(list, at + 12))
    if id.isNone or size.isNone: break
    let start = at + 16
    let len = int(size.get)
    if start > list.len or len > list.len - start: break
    let m = if id.get == 9: gainMap(list.toOpenArray(start, start + len - 1)) else: none(GainMap)
    if m.isSome: result[0].add m.get
    else:
      let n = opcodeName(id.get)
      if n notin result[1]: result[1].add n
    at = start + len
