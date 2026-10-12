## Gamut checks against a proof (output) profile: View › Gamut Warning.

import std/[options, math]
import ./clut, ./pipeline, ./profile, ./builtin, ./transform

const DefaultGamutThreshold* = 4.0'f32 ## ΔE76 above which a colour counts as out of gamut.

type
  GamutCheck* = ref object
    ## A colour is out of gamut when sending it to the proof device (relative colorimetric,
    ## clipping) and back to Lab moves it by more than the threshold. Tabulated over the source
    ## device space (33³ for 3 channels, 17⁴ for 4, exact otherwise).
    inputs*: int
    grid: Option[Clut]
    direct, via: Pipeline
    threshold*: float32

proc newGamutCheck*(src, proof: Profile; threshold: float32): GamutCheck =
  ## Raises `CmsError` when the profiles cannot be linked.
  let lab = bLabD50.profile
  let direct = newPipeline(src.channels, linkStages(src, lab, RelativeColorimetric, false))
  var via = linkStages(src, proof, RelativeColorimetric, false)
  via.add simpleStage(skClamp01)
  via.add linkStages(proof, lab, RelativeColorimetric, false)
  let viaP = newPipeline(src.channels, via)
  let de = proc(p: openArray[float32]; o: var openArray[float32]) =
    var a, b: PipeVec
    direct.eval(p, a)
    viaP.eval(p, b)
    o[0] = sqrt(((a[0] - b[0]) * 100.0'f32) ^ 2 + ((a[1] - b[1]) * 255.0'f32) ^ 2 + ((a[2] - b[2]) * 255.0'f32) ^ 2)
  let n = src.channels
  var grid = none(Clut)
  case n
  of 1: grid = some(sampleClut(@[256], 1, de))
  of 3: grid = some(sampleClut(@[33, 33, 33], 1, de))
  of 4: grid = some(sampleClut(@[17, 17, 17, 17], 1, de))
  else: discard
  GamutCheck(inputs: n, grid: grid, direct: direct, via: viaP, threshold: threshold)

proc deltaE*(g: GamutCheck; px: openArray[float32]): float32 =
  ## ΔE76 between the colour and its proof round trip.
  var o: array[1, float32]
  if g.grid.isSome:
    var c: PipeVec
    for d in 0 ..< g.inputs: c[d] = clamp(px[d], 0.0'f32, 1.0'f32)
    g.grid.get.eval(c.toOpenArray(0, g.inputs - 1), o)
  else:
    var a, b: PipeVec
    g.direct.eval(px, a)
    g.via.eval(px, b)
    o[0] = sqrt(((a[0] - b[0]) * 100.0'f32) ^ 2 + ((a[1] - b[1]) * 255.0'f32) ^ 2 + ((a[2] - b[2]) * 255.0'f32) ^ 2)
  o[0]

proc outOfGamut*(g: GamutCheck; px: openArray[float32]): bool = g.deltaE(px) > g.threshold
