## Geometry primitives shared by every PhotoCraft module.
##
## Document space is integer pixels with the origin at the canvas top-left.
## Layers may extend beyond the canvas (negative coordinates are valid).
##
## Port of `crates/geom`. Coordinates keep the Rust crate's i32 range: rectangle
## edges saturate at `int32.low`/`int32.high` instead of overflowing.

import std/[math, hashes, options]
export options

const TileSize* = 256 ## Edge length of a raster tile in pixels.

type
  Size* = object
    width*, height*: int

  Rect* = object
    ## Integer rectangle, half-open: `[x0, x1) × [y0, y1)`.
    x0*, y0*, x1*, y1*: int

  TileCoord* = object
    ## Index of a tile in the infinite tile grid.
    tx*, ty*: int

  Point* = object
    x*, y*: float64

  Affine* = object
    ## 2D affine transform `[a c e; b d f; 0 0 1]` (column vectors).
    m*: array[6, float64]

const
  I32Min = int(int32.low)
  I32Max = int(int32.high)

proc clampI32(v: int): int {.inline.} = max(I32Min, min(I32Max, v))
proc satAdd*(a, b: int): int {.inline.} =
  ## Saturating add in the i32 range (the Rust crate's coordinate type).
  clampI32(a + b)
proc satSub*(a, b: int): int {.inline.} = clampI32(a - b)
proc asI32*(v: float64): int {.inline.} =
  ## Rust `v as i32`: truncates toward zero, saturates at the i32 range, NaN → 0.
  if v != v: 0
  elif v >= float64(I32Max): I32Max
  elif v <= float64(I32Min): I32Min
  else: int(v)
proc asUsize*(v: float64): int {.inline.} =
  ## Rust `v as usize` (on 64-bit, capped at `high(int)`): NaN and negatives → 0.
  if v != v or v <= 0: 0
  elif v >= 9.223372036854775807e18: high(int)
  else: int(v)

# ---- Size ----

proc size*(width, height: int): Size {.inline.} = Size(width: width, height: height)
proc area*(s: Size): int64 = int64(s.width) * int64(s.height)
proc isEmpty*(s: Size): bool = s.width <= 0 or s.height <= 0
proc hash*(s: Size): Hash = hash((s.width, s.height))

# ---- Rect ----

const EmptyRect* = Rect(x0: 0, y0: 0, x1: 0, y1: 0)

proc rect*(x0, y0, x1, y1: int): Rect {.inline.} = Rect(x0: x0, y0: y0, x1: x1, y1: y1)
proc rectXywh*(x, y: int; w, h: int): Rect = rect(x, y, satAdd(x, w), satAdd(y, h))
proc rectFromSize*(s: Size): Rect = rectXywh(0, 0, s.width, s.height)
proc width*(r: Rect): int = (if r.x1 > r.x0: r.x1 - r.x0 else: 0)
proc height*(r: Rect): int = (if r.y1 > r.y0: r.y1 - r.y0 else: 0)
proc size*(r: Rect): Size = Size(width: r.width, height: r.height)
proc isEmpty*(r: Rect): bool = r.x1 <= r.x0 or r.y1 <= r.y0
proc containsRect*(r, other: Rect): bool =
  ## `other` lies entirely inside `r` (an empty `other` is contained in anything).
  other.isEmpty or (other.x0 >= r.x0 and other.y0 >= r.y0 and other.x1 <= r.x1 and other.y1 <= r.y1)
proc contains*(r: Rect; x, y: int): bool = x >= r.x0 and x < r.x1 and y >= r.y0 and y < r.y1
proc intersect*(a, o: Rect): Rect =
  let r = rect(max(a.x0, o.x0), max(a.y0, o.y0), min(a.x1, o.x1), min(a.y1, o.y1))
  if r.isEmpty: EmptyRect else: r
proc union*(a, o: Rect): Rect =
  if a.isEmpty: return o
  if o.isEmpty: return a
  rect(min(a.x0, o.x0), min(a.y0, o.y0), max(a.x1, o.x1), max(a.y1, o.y1))
proc translate*(r: Rect; dx, dy: int): Rect =
  rect(satAdd(r.x0, dx), satAdd(r.y0, dy), satAdd(r.x1, dx), satAdd(r.y1, dy))
proc inflate*(r: Rect; d: int): Rect =
  rect(satSub(r.x0, d), satSub(r.y0, d), satAdd(r.x1, d), satAdd(r.y1, d))
proc hash*(r: Rect): Hash = hash((r.x0, r.y0, r.x1, r.y1))
proc `$`*(r: Rect): string = "Rect(" & $r.x0 & ", " & $r.y0 & ", " & $r.x1 & ", " & $r.y1 & ")"

# ---- TileCoord ----

proc tileCoord*(tx, ty: int): TileCoord {.inline.} = TileCoord(tx: tx, ty: ty)
proc divEuclid*(a, b: int): int {.inline.} =
  ## Euclidean division (rounds towards negative infinity for positive `b`).
  result = a div b
  if a mod b < 0:
    if b > 0: dec result else: inc result
proc remEuclid*(a, b: int): int {.inline.} =
  result = a mod b
  if result < 0: result += abs(b)
proc containing*(_: typedesc[TileCoord]; x, y: int): TileCoord =
  tileCoord(divEuclid(x, TileSize), divEuclid(y, TileSize))
proc tileContaining*(x, y: int): TileCoord = TileCoord.containing(x, y)
proc rect*(t: TileCoord): Rect = rectXywh(t.tx * TileSize, t.ty * TileSize, TileSize, TileSize)
proc hash*(t: TileCoord): Hash = hash((t.tx, t.ty))
proc `<`*(a, b: TileCoord): bool = (a.tx, a.ty) < (b.tx, b.ty)
proc cmp*(a, b: TileCoord): int = cmp((a.tx, a.ty), (b.tx, b.ty))

iterator tiles*(r: Rect): TileCoord =
  ## All tiles overlapping this rect (row-major).
  if not r.isEmpty:
    let a = tileContaining(r.x0, r.y0)
    let b = tileContaining(r.x1 - 1, r.y1 - 1)
    for ty in a.ty .. b.ty:
      for tx in a.tx .. b.tx:
        yield tileCoord(tx, ty)

proc tileList*(r: Rect): seq[TileCoord] =
  for t in r.tiles: result.add t

# ---- Point / Affine ----

proc point*(x, y: float64): Point {.inline.} = Point(x: x, y: y)

const IdentityAffine* = Affine(m: [1.0, 0.0, 0.0, 1.0, 0.0, 0.0])

proc affine*(m: array[6, float64]): Affine = Affine(m: m)
proc translateAffine*(dx, dy: float64): Affine = Affine(m: [1.0, 0.0, 0.0, 1.0, dx, dy])
proc scaleAffine*(s: float64): Affine = Affine(m: [s, 0.0, 0.0, s, 0.0, 0.0])
proc rotateAffine*(radians: float64): Affine =
  let s = sin(radians)
  let c = cos(radians)
  Affine(m: [c, s, -s, c, 0.0, 0.0])

proc mul*(self, o: Affine): Affine =
  let (a, b, c, d, e, f) = (self.m[0], self.m[1], self.m[2], self.m[3], self.m[4], self.m[5])
  let (oa, ob, oc, od, oe, off) = (o.m[0], o.m[1], o.m[2], o.m[3], o.m[4], o.m[5])
  Affine(m: [a * oa + c * ob, b * oa + d * ob, a * oc + c * od, b * oc + d * od,
             a * oe + c * off + e, b * oe + d * off + f])

proc then*(self, other: Affine): Affine =
  ## `self * other`: apply `self` first, then `other`.
  other.mul(self)

proc apply*(t: Affine; p: Point): Point =
  point(t.m[0] * p.x + t.m[2] * p.y + t.m[4], t.m[1] * p.x + t.m[3] * p.y + t.m[5])

proc determinant*(t: Affine): float64 = t.m[0] * t.m[3] - t.m[1] * t.m[2]

proc inverse*(t: Affine): Option[Affine] =
  ## Inverse, or none when the transform is (nearly) singular.
  let det = t.determinant
  if abs(det) < 1e-12:
    return none(Affine)
  let (a, b, c, d, e, f) = (t.m[0], t.m[1], t.m[2], t.m[3], t.m[4], t.m[5])
  let inv = 1.0 / det
  some Affine(m: [d * inv, -b * inv, -c * inv, a * inv, (c * f - d * e) * inv, (b * e - a * f) * inv])

proc isInvertible*(t: Affine): bool = abs(t.determinant) >= 1e-12
