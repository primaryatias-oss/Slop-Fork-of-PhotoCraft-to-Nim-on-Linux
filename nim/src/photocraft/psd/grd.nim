## Photoshop gradient files (`.grd`, version 5): `8BGR`, u16 version, then a versioned descriptor
## whose `GrdL` list holds `Grdn` objects with a `Grad` object (name, form, colour stops `Clrs`,
## transparency stops `Trns`). Colours are returned in the model they were stored in.

import std/[options, math]
import ./io, ./descriptor

const
  GrdMaxGradients* = 10_000
  GrdMaxStops* = 1_000

type
  GrdColorKind* = enum gcRgb, gcHsb, gcCmyk, gcLab, gcGray, gcForeground, gcBackground
  GrdColor* = object
    kind*: GrdColorKind
    v*: array[4, float32]  ## RGB 0..1; HSB deg, 0..1, 0..1; CMYK 0..1 ink; Lab L, a, b; gray 0..1 ink
  GrdStop* = object
    location*, midpoint*: float32
    color*: GrdColor
  GrdGradient* = object
    name*: string
    noise*: bool
    stops*: seq[GrdStop]
    opacity*: seq[(float32, float32, float32)]  ## (location, opacity, midpoint), all 0..1

proc num(d: Descriptor; k: string): Option[float64] =
  let v = d.getDouble(k)
  if v.isSome and v.get == v.get and abs(v.get) != Inf: v else: none(float64)
proc f(d: Descriptor; k: string): float32 = float32(d.num(k).get(0.0))
proc enumIs(d: Descriptor; k, v: string): bool = d.getEnum(k) == some(v)

proc grdColor(c: Descriptor): Option[GrdColor] =
  case c.classId.s
  of "RGBC": some(GrdColor(kind: gcRgb, v: [c.f("Rd  ") / 255, c.f("Grn ") / 255, c.f("Bl  ") / 255, 0]))
  of "HSBC": some(GrdColor(kind: gcHsb, v: [c.f("H   "), c.f("Strt") / 100, c.f("Brgh") / 100, 0]))
  of "CMYC": some(GrdColor(kind: gcCmyk, v: [c.f("Cyn ") / 100, c.f("Mgnt") / 100, c.f("Ylw ") / 100, c.f("Blck") / 100]))
  of "LbCl": some(GrdColor(kind: gcLab, v: [c.f("Lmnc"), c.f("A   "), c.f("B   "), 0]))
  of "Grsc": some(GrdColor(kind: gcGray, v: [c.f("Gry ") / 100, 0, 0, 0]))
  else: none(GrdColor)

proc location(d: Descriptor): float32 = clamp(d.f("Lctn") / 4096, 0'f32, 1'f32)
proc midpoint(d: Descriptor): float32 = clamp(float32(d.num("Mdpn").get(50.0)) / 100, 0'f32, 1'f32)

proc gradient(g: Descriptor): GrdGradient =
  result.name = g.getText("Nm  ").get("")
  result.noise = g.enumIs("GrdF", "ClNs")
  let clrs = g.getList("Clrs").get(@[])
  for i, v in clrs:
    if i >= GrdMaxStops: break
    if v.kind != vkDescriptor: continue
    let s = v.desc
    let c = if s.enumIs("Type", "FrgC"): some(GrdColor(kind: gcForeground))
            elif s.enumIs("Type", "BckC"): some(GrdColor(kind: gcBackground))
            else:
              let cd = s.getDesc("Clr ")
              if cd.isSome: grdColor(cd.get) else: none(GrdColor)
    if c.isSome: result.stops.add GrdStop(location: location(s), midpoint: midpoint(s), color: c.get)
  let trns = g.getList("Trns").get(@[])
  for i, v in trns:
    if i >= GrdMaxStops: break
    if v.kind != vkDescriptor: continue
    let s = v.desc
    result.opacity.add (location(s), clamp(float32(s.num("Opct").get(100.0)) / 100, 0'f32, 1'f32), midpoint(s))

proc parseGrd*(data: openArray[byte]): seq[GrdGradient] =
  if data.len < 4: raise eof(0, 4)
  let sig = toFourCC(data)
  if sig != "8BGR": raise badSignature("8BGR", sig)
  if data.len < 6: raise eof(4, 2)
  let version = beU16(data, 4)
  if version != 5: raise unsupported("gradient file version " & $version & " (only version 5, Photoshop 6 and later)")
  let (vd, _) = parseVersionedPrefix(data.toOpenArray(6, data.len - 1))
  let list = vd.descriptor.getList("GrdL").get(@[])
  for i, v in list:
    if i >= GrdMaxGradients: break
    if v.kind == vkDescriptor:
      let g = v.desc.getDesc("Grad")
      if g.isSome: result.add gradient(g.get)
  if result.len == 0: raise invalid("the gradient file holds no gradients")

proc writeGrd*(gradients: openArray[GrdGradient]): seq[byte] =
  ## A version 5 `.grd` file (RGB / foreground / background stops).
  var items: seq[Value]
  for g in gradients:
    var stops: seq[Value]
    for s in g.stops:
      var d = newDescriptor("Clrt")
      var ty = "UsrS"
      case s.color.kind
      of gcForeground: ty = "FrgC"
      of gcBackground: ty = "BckC"
      else:
        let rgb = if s.color.kind == gcRgb: s.color.v else: [0'f32, 0, 0, 0]
        d = d.with("Clr ", vDesc(newDescriptor("RGBC").with("Rd  ", vDouble(float64(rgb[0]) * 255))
          .with("Grn ", vDouble(float64(rgb[1]) * 255)).with("Bl  ", vDouble(float64(rgb[2]) * 255))))
      stops.add vDesc(d.with("Type", vEnum("Clry", ty)).with("Lctn", vInt(int32(round(s.location * 4096))))
        .with("Mdpn", vInt(int32(round(s.midpoint * 100)))))
    var trns: seq[Value]
    for (l, o, m) in g.opacity:
      trns.add vDesc(newDescriptor("TrnS").with("Opct", vUnit("#Prc", float64(o) * 100))
        .with("Lctn", vInt(int32(round(l * 4096)))).with("Mdpn", vInt(int32(round(m * 100)))))
    let grad = newDescriptor("Grdn").with("Nm  ", vText(g.name))
      .with("GrdF", vEnum("GrdF", if g.noise: "ClNs" else: "CstS")).with("Intr", vDouble(4096))
      .with("Clrs", vList(stops)).with("Trns", vList(trns))
    items.add vDesc(newDescriptor("Grdn").with("Grad", vDesc(grad)))
  result.put("8BGR")
  result.putU16(5)
  result.add versioned(newDescriptor("null").with("GrdL", vList(items))).toBytes
