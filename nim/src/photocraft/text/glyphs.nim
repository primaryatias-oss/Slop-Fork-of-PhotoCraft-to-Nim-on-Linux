## Glyphs panel support: the characters a face maps (its `cmap`), Photoshop's "Show" categories,
## and small glyph previews (port of `photocraft_text::glyphs`).

import std/[options, unicode, algorithm, math]
import ../geom, ../color, ../raster, ../doc
import ../font/sfnt
import fonts, layout, render, uprops
import ../text as engine

const Categories* = ["Entire Font", "Basic Latin", "Latin 1", "Latin Extended-A", "Latin Extended-B", "Greek",
                     "Cyrillic", "Punctuation", "Numbers", "Currency", "Symbols", "Math Symbols", "Arrows",
                     "Ornaments"]
  ## The Glyphs panel's "Show" categories, in menu order.

proc inCategory*(c: int; category: string): bool =
  ## Whether `c` belongs to a `Categories` entry (unknown names match everything).
  let u = c
  case category
  of "Basic Latin": u in 0x20 .. 0x7E
  of "Latin 1": u in 0xA0 .. 0xFF
  of "Latin Extended-A": u in 0x100 .. 0x17F
  of "Latin Extended-B": u in 0x180 .. 0x24F
  of "Greek": u in 0x370 .. 0x3FF or u in 0x1F00 .. 0x1FFF
  of "Cyrillic": u in 0x400 .. 0x52F
  of "Punctuation":
    (u < 128 and char(u) in {'!' .. '/', ':' .. '@', '[' .. '`', '{' .. '~'} and
      char(u) notin {'$', '+', '<', '=', '>', '^', '`', '|', '~'}) or
      u in [0xA1, 0xA7, 0xAB, 0xB6, 0xB7, 0xBB, 0xBF] or u in 0x2010 .. 0x2027 or u in 0x2030 .. 0x205E or
      u in 0x2E00 .. 0x2E7F or u in 0x3000 .. 0x303F
  of "Numbers": generalCategory(u) in {gcNd, gcNl, gcNo} or u in 0x2150 .. 0x218F
  of "Currency": u in [ord('$'), 0xA2, 0xA3, 0xA4, 0xA5] or u in 0x20A0 .. 0x20CF
  of "Symbols":
    u in [0xA9, 0xAE, 0xB0, 0xA6, 0xA8, 0xAF, 0xB4, 0xB8, 0x2122, 0x2120, 0x2116, 0x212E, ord('^'), ord('`'),
          ord('|'), ord('~')] or u in 0x2100 .. 0x214F or u in 0x2300 .. 0x23FF or u in 0x2500 .. 0x25FF
  of "Math Symbols":
    u in [ord('+'), ord('<'), ord('='), ord('>'), 0xB1, 0xD7, 0xF7, 0xAC, 0xB5, 0xB9, 0xB2, 0xB3, 0xBC, 0xBD, 0xBE] or
      u in 0x2070 .. 0x209F or u in 0x2200 .. 0x22FF or u in 0x27C0 .. 0x27EF or u in 0x2980 .. 0x2AFF
  of "Arrows": u in 0x2190 .. 0x21FF or u in 0x27F0 .. 0x27FF or u in 0x2900 .. 0x297F or u in 0x2B00 .. 0x2BFF
  of "Ornaments": u in 0x2600 .. 0x27BF or u in 0x1F300 .. 0x1FAFF or u in 0xE000 .. 0xF8FF
  else: true

proc charmap*(db: FontDb; family: string; weight: uint16; italic: bool): seq[int] =
  ## Characters mapped by the face of `family` nearest to `weight`/`italic`, ascending (controls
  ## and whitespace other than the space excluded). Empty for an unknown family.
  let fi = db.familyIndex(family)
  if fi < 0: return
  let faces = db.familyFaces(fi)
  var best = -1
  var bestKey = high(uint64)
  for i, f in faces:
    let it = f.style != fsNormal
    let d = abs(f.weight - float32(weight))
    let dk = if d != d: 0'u64 elif d >= 4294967295'f32: 4294967295'u64 else: uint64(d)
    let key = (if it != italic: 10_000'u64 else: 0'u64) + dk
    if key < bestKey:
      bestKey = key
      best = i
  if best < 0: return
  let f = faces[best].tryLoad
  if f.isNone: return
  for (u, _) in f.get.mappings:
    if u > 0x10FFFF or u in 0xD800 .. 0xDFFF: continue
    let ctrl = generalCategory(u) == gcCc
    if u == 0x20 or not (ctrl or isWhiteSpace(u) or u in 0xFE00 .. 0xFE0F): result.add u
  result.sort()
  var k = 0
  for i in 0 ..< result.len:
    if i == 0 or result[i] != result[k - 1]:
      result[k] = result[i]
      inc k
  result.setLen(k)

proc charmapForStyle*(db: FontDb; style: CharStyle): seq[int] =
  ## Character coverage of the exact named face used by the type tool.
  var st = style
  db.selectNamedFace(st)
  db.charmap(st.fontFamily, st.weight, st.italic)

proc preview*(e: engine.TextEngine; style: CharStyle; c: int; px: int): (int, seq[uint8]) =
  ## An 8-bit coverage preview of one character in a `px`-pixel em box, centred horizontally on
  ## the baseline at 80 % of the box: (width = height = `px`, alpha bytes).
  let px = clamp(px, 4, 512)
  let text = Rune(c).toUTF8
  var st = style
  st.sizePt = float32(px) * 0.72'f32
  st.underline = false
  st.strikethrough = false
  st.tracking = 0
  st.baselineShiftPt = 0
  var layer = defaultTextLayer()
  layer.text = text
  layer.runs = @[TextRun(len: text.len, style: st)]
  let l = e.layout(layer, 72)
  let b = l.bounds
  let w = if b.isSome: b.get[2] - b.get[0] else: 0'f32
  let x0 = if b.isSome: b.get[0] else: 0'f32
  let tx = (float32(px) - w) / 2 - x0
  let r = rasterizeWarped(l, Affine(m: [1.0, 0, 0, 1, float64(tx), float64(float32(px) * 0.78'f32)]),
                          pixelFormat(cmRgb, stU8, true), aaSmooth, none(TextWarper))
  var rgba = newSeq[array[4, uint8]](px * px)
  r.surface.readRgba8Into(rect(0, 0, px, px), rgba)
  var alpha = newSeq[uint8](px * px)
  for i, p in rgba: alpha[i] = p[3]
  (px, alpha)
