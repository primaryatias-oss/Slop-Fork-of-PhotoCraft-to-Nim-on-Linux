## The document model PhotoCraft imports from Affinity files (types of `affinity/src/model.rs` and
## `paint.rs`) and the `Reader` the mapping modules share: spreads, artboards, layers, groups,
## curves, shapes, text and images, with transforms composed into document space.

import std/[options, tables, sets, algorithm, math]
import ./container, ./stream
import ../rmath

type
  AfAffine* = object
    ## Affine map `x' = a·x + c·y + e`, `y' = b·x + d·y + f` (kurbo's coefficient order).
    m*: array[6, float64]

  AfPoint* = object
    x*, y*: float64

  AfRect* = object
    x0*, y0*, x1*, y1*: float64

  AfSubPath* = object
    ## One cubic Bézier subpath, in document pixels.
    start*: AfPoint
    segments*: seq[array[3, AfPoint]] ## (control 1, control 2, end) per segment
    closed*: bool

  AfPath* = object
    ## An outline in document pixels.
    subpaths*: seq[AfSubPath]

  AfBlend* = enum
    ## Affinity's blend modes, numbered by the `Blnd` enum.
    abNormal, abPassThrough, abDarken, abDarkerColor, abMultiply, abColorBurn, abLighten, abLighterColor,
    abScreen, abColorDodge, abAdd, abOverlay, abSoftLight, abHardLight, abVividLight, abPinLight,
    abLinearLight, abHardMix, abDifference, abExclusion, abSubtract, abHue, abSaturation, abLuminosity,
    abColor, abAverage, abNegation, abReflect, abGlow, abErase

  # ---- paint (paint.rs)

  AfColorKind* = enum
    acRgb, acCmyk, acGray, acLab

  AfColor* = object
    ## A colour as stored: RGB and grey values are encoded in the document's RGB space (sRGB unless
    ## the document embeds another profile), CMYK values go through its CMYK profile. Alpha is
    ## straight. Lab is CIE L*a*b* (D50).
    a*: float64 ## alpha
    case kind*: AfColorKind
    of acRgb: r*, g*, b*: float64
    of acCmyk: c*, mg*, y*, k*: float64
    of acGray: v*: float64
    of acLab: l*, la*, lb*: float64

  AfGradientKind* = enum
    agLinear
    agRadial  ## elliptical and radial: distance from the centre in unit space
    agConical ## angle around the centre

  AfStop* = object
    offset*: float64
    color*: AfColor
    bias*: float64 ## midpoint bias for the segment from this stop to the next (0.5 = even blend)

  AfGradient* = object
    kind*: AfGradientKind
    stops*: seq[AfStop]
    transform*: AfAffine
      ## Unit space to document pixels: linear runs from unit (0,0) to (1,0); radial is the unit
      ## circle; conical starts along unit +x and turns towards unit +y.

  AfPaintKind* = enum
    apNone, apSolid, apGradient

  AfPaint* = object
    case kind*: AfPaintKind
    of apNone: discard
    of apSolid: color*: AfColor
    of apGradient: gradient*: AfGradient

  AfCap* = enum
    capButt, capRound, capSquare

  AfJoin* = enum
    joinMiter, joinRound, joinBevel

  AfStrokeAlign* = enum
    strokeCenter, strokeInside, strokeOutside

  AfStroke* = object
    paint*: AfPaint
    width*: float64 ## in document pixels
    cap*: AfCap
    join*: AfJoin
    miterLimit*: float64
    align*: AfStrokeAlign
    dash*: Option[(seq[float64], float64)] ## dash and gap lengths in document pixels, and the phase
    behind*: bool ## painted below the fill instead of above it

  # ---- text

  AfTextRun* = object
    ## A run of text with one set of character attributes.
    text*: string
    postscript*, family*: string ## PostScript name and family of the chosen font
    weight*: int64 ## CSS weight (400 regular, 700 bold)
    italic*: bool
    size*: float64 ## font size in document pixels, before the node's transform
    tracking*: float64 ## in em
    leading*: Option[float64] ## fixed line pitch in document pixels, when overriding automatic leading
    fill*: AfPaint

  AfAlign* = enum
    alLeft, alCenter, alRight, alJustify

  AfText* = object
    runs*: seq[AfTextRun]
    align*: AfAlign
    anchor*: AfPoint
      ## Artistic text: the first baseline's anchor point in node space (start, centre or end of
      ## the line according to `align`). Frame text: unused (the frame box is `frame`).
    frame*: Option[AfRect]
    transform*: AfAffine ## node space to document pixels

  AfPixelsKind* = enum
    pxRgba8   ## straight-alpha RGBA, 8 bits per channel, row by row
    pxEncoded ## the original file a placed image was made from (PNG, JPEG…), as embedded

  AfPixels* = object
    kind*: AfPixelsKind
    data*: seq[byte]

  AfImage* = object
    width*, height*: uint32
    pixels*: AfPixels
    transform*: AfAffine ## pixel space (0..w, 0..h) to document pixels

  AfKindTag* = enum
    nkLayer       ## layer container (`Scop`)
    nkGroup
    nkArtboard    ## a rectangle that clips its children; `background` paints behind them
    nkShape
      ## Curve, parametric shape or compound shape: an outline with its fills and strokes.
      ## Children of a shape are clipped by its outline (painted over its fills, under its strokes).
    nkText
    nkImage
    nkUnsupported ## something this reader does not import; its children are still imported

  AfKind* = object
    case tag*: AfKindTag
    of nkArtboard:
      rect*: AfRect
      background*: seq[AfPaint]
    of nkShape:
      path*: AfPath
      fills*: seq[AfPaint]
      strokes*: seq[AfStroke]
      evenOdd*: bool
    of nkText: text*: AfText
    of nkImage: image*: AfImage
    else: discard

  AfNode* = object
    class*: Tag
    name*: string
    visible*, locked*: bool
    opacity*: float64
    blend*: AfBlend
    kind*: AfKind
    mask*: Option[AfPath] ## vector mask in document pixels: the node shows only inside it (nonzero)
    pixelMask*: Option[AfImage]
      ## grey levels (white shows the node, black hides it); outside it the node is hidden
    children*: seq[AfNode] ## bottom to top

  AfSpread* = object
    bounds*: AfRect ## the canvas, or the union of the artboards, in document pixels
    pages*: seq[AfRect] ## pages of a Publisher-style spread (one, or two facing pages)
    transparent*: bool ## transparent background; otherwise the page is white
    nodes*: seq[AfNode] ## bottom to top

  AfDocument* = object
    dpi*: float64 ## pixels per inch: all geometry is in document pixels at this resolution
    spreads*: seq[AfSpread]
    savedBy*: Option[string] ## application that last saved the file (`Affinity 3.0.2`)
    warnings*: seq[string]
      ## content that was not imported or was approximated, one line per kind, deduplicated

  Reader* = object
    s*: Stream
    archive*: Archive
    dpi*: float64
    warnings: Table[string, int]
    nodes*: int
    active*: HashSet[ObjId]

const
  Identity* = AfAffine(m: [1.0, 0.0, 0.0, 1.0, 0.0, 0.0])

proc fromXfrm*(m: openArray[float64]): AfAffine =
  ## Affinity's `Xfrm`, stored row by row: (a, c, e, b, d, f).
  AfAffine(m: [m[0], m[3], m[1], m[4], m[2], m[5]])

proc then*(self, outer: AfAffine): AfAffine =
  let (a, b, c, d, e, f) = (self.m[0], self.m[1], self.m[2], self.m[3], self.m[4], self.m[5])
  let (oa, ob, oc, od, oe, of0) = (outer.m[0], outer.m[1], outer.m[2], outer.m[3], outer.m[4], outer.m[5])
  AfAffine(m: [oa * a + oc * b, ob * a + od * b, oa * c + oc * d, ob * c + od * d, oa * e + oc * f + oe, ob * e + od * f + of0])

proc pt*(x, y: float64): AfPoint {.inline.} = AfPoint(x: x, y: y)

proc `==`*(a, b: AfPoint): bool {.inline.} = a.x == b.x and a.y == b.y

proc apply*(t: AfAffine; p: AfPoint): AfPoint =
  let (a, b, c, d, e, f) = (t.m[0], t.m[1], t.m[2], t.m[3], t.m[4], t.m[5])
  AfPoint(x: a * p.x + c * p.y + e, y: b * p.x + d * p.y + f)

proc scale*(t: AfAffine): float64 =
  ## Geometric mean of the axis scales: how much a stroke width or font size grows.
  sqrt(abs(t.m[0] * t.m[3] - t.m[1] * t.m[2]))

proc finite(v: float64): bool {.inline.} = v == v and abs(v) != Inf

proc isFinite*(t: AfAffine): bool =
  for v in t.m:
    if not finite(v): return false
  true

proc transformed*(p: AfPath; m: AfAffine): AfPath =
  for s in p.subpaths:
    var o = AfSubPath(start: m.apply(s.start), closed: s.closed)
    for seg in s.segments: o.segments.add [m.apply(seg[0]), m.apply(seg[1]), m.apply(seg[2])]
    result.subpaths.add o

proc alpha*(c: AfColor): float64 = c.a

proc `==`*(a, b: AfColor): bool =
  if a.kind != b.kind or a.a != b.a: return false
  case a.kind
  of acRgb: a.r == b.r and a.g == b.g and a.b == b.b
  of acCmyk: a.c == b.c and a.mg == b.mg and a.y == b.y and a.k == b.k
  of acGray: a.v == b.v
  of acLab: a.l == b.l and a.la == b.la and a.lb == b.lb

proc rgb*(r, g, b, a: float64): AfColor = AfColor(kind: acRgb, r: r, g: g, b: b, a: a)

proc `==`*(a, b: AfAffine): bool = a.m == b.m

proc `==`*(a, b: AfStop): bool = a.offset == b.offset and a.color == b.color and a.bias == b.bias

proc `==`*(a, b: AfGradient): bool = a.kind == b.kind and a.stops == b.stops and a.transform == b.transform

proc `==`*(a, b: AfPaint): bool =
  if a.kind != b.kind: return false
  case a.kind
  of apNone: true
  of apSolid: a.color == b.color
  of apGradient: a.gradient == b.gradient

proc isNone*(p: AfPaint): bool = p.kind == apNone

proc halfPoint*(s: AfStop): float64 =
  ## Where, between this stop (0) and the next (1), the blend reaches half way: the bias curve
  ## fitted to Affinity's renders is `t^k` (bias ≥ 0.5) or `1-(1-t)^k`, `k = 1 + 8·|bias-0.5|`.
  let k = 1.0 + 8.0 * abs(s.bias - 0.5)
  let h = pow(0.5, 1.0 / k)
  if s.bias >= 0.5: h else: 1.0 - h

proc newReader*(s: sink Stream; archive: sink Archive): Reader =
  Reader(s: s, archive: archive, dpi: 72.0)

proc warn*(r: var Reader; what: string) =
  r.warnings.mgetOrPut(what, 0) += 1

proc takeWarnings*(r: var Reader): seq[string] =
  ## The warnings sorted (byte order), with repeat counts.
  var keys: seq[string]
  for k in r.warnings.keys: keys.add k
  keys.sort(cmp)
  for k in keys:
    let n = r.warnings[k]
    result.add(if n > 1: k & " (" & $n & "×)" else: k)

proc rect*(v: openArray[float64]): AfRect = AfRect(x0: v[0], y0: v[1], x1: v[2], y1: v[3])

proc union*(a, b: AfRect): AfRect =
  AfRect(x0: rmin(a.x0, b.x0), y0: rmin(a.y0, b.y0), x1: rmax(a.x1, b.x1), y1: rmax(a.y1, b.y1))

proc xfrm*(r: Reader; id: ObjId): AfAffine =
  ## The node's own transform (`Xfrm`), identity when absent.
  let v = r.s.floatsN(id, "Xfrm", 6)
  if v.isSome: fromXfrm(v.get) else: Identity

proc describe*(class: Tag): string =
  ## A readable name for a layer class in warnings.
  case $class
  of "PCrv": "curve"
  of "ShpN", "SNEN", "SNRR", "ShRN": "shape"
  of "Comp": "compound"
  of "Rstr": "pixel"
  of "ImgN": "image"
  of "EmbN": "embedded document"
  of "MPIN": "master page instance"
  of "FRst": "fill"
  of "MRst": "mask"
  of "Slic", "SlcP": "slice"
  of "3DLA": "3D"
  else:
    let v = uint32(class)
    if ((v shr 8) and 0xFF) == uint32('R') and (v and 0xFF) == uint32('A'): "adjustment" else: "unknown"
