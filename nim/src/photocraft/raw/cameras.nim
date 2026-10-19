## Camera profiles measured by PhotoCraft (port of `raw/src/cameras.rs`), for raw formats that
## carry no colour description of their own (DNG does; Nikon NEF does not).
##
## Clean-room: every number comes from our own measurements of the camera's output. A profile
## gives the image area inside the sensor data (the camera's own JPEG crop), the saturation level,
## a ForwardMatrix (white-balanced camera → XYZ D50, DNG semantics) and a default tone curve.
## Bodies whose full profile is not measured yet can still get their measured image area.

import std/[options, strutils, math]
import ./color, ./sensor

const
  D4Forward*: Mat3 = [[0.644598, 0.0703598, 0.249262], [0.342249, 0.518929, 0.138822], [0.0568265, -0.269568, 1.03795]]
  D4Color*: Mat3 = [[0.895936, -0.218092, -0.185988], [-1.09736, 2.06898, -0.0131889], [-0.26977, 0.393884, 0.687829]]

let D4Tone*: seq[array[2, float32]] = @[
  [0.005524272'f32, 0.00285028'f32], [0.006569503'f32, 0.00357083'f32], [0.0078125'f32, 0.00447912'f32],
  [0.009290681'f32, 0.00605813'f32], [0.01104854'f32, 0.00841445'f32], [0.01313901'f32, 0.011536'f32],
  [0.015625'f32, 0.0165399'f32], [0.01858136'f32, 0.0230234'f32], [0.02209709'f32, 0.0319205'f32],
  [0.02627801'f32, 0.0434858'f32], [0.03125'f32, 0.0582138'f32], [0.03716272'f32, 0.0771963'f32],
  [0.04419417'f32, 0.0993894'f32], [0.05255603'f32, 0.125058'f32], [0.0625'f32, 0.157207'f32],
  [0.07432544'f32, 0.194929'f32], [0.08838835'f32, 0.236109'f32], [0.1051121'f32, 0.279155'f32],
  [0.125'f32, 0.329365'f32], [0.1486509'f32, 0.388405'f32], [0.1767767'f32, 0.457944'f32],
  [0.2102241'f32, 0.52456'f32], [0.25'f32, 0.598051'f32], [0.2973018'f32, 0.668794'f32],
  [0.3535534'f32, 0.749954'f32], [0.4204482'f32, 0.815443'f32], [0.5'f32, 0.870167'f32],
  [0.5946036'f32, 0.907869'f32], [float32(0.7071067811865476), 0.939351'f32], [0.8408964'f32, 0.970567'f32],
  [1'f32, 1'f32]]

type
  Profile = object
    make, model: string
    size: (int, int)            ## sensor data size the profile was measured on
    crop: (int, int, int, int)  ## the image area: x, y, width, height
    white: float32
    blackZero: bool             ## the data is black-subtracted in camera
  ImageArea = object
    make, model: string
    size: (int, int)
    crop: (int, int, int, int)

let
  Profiles = [Profile(make: "NIKON CORPORATION", model: "NIKON D4", size: (4992, 3292), crop: (8, 6, 4928, 3280),
                      white: 15520, blackZero: true)]
  ImageAreas = [
    ImageArea(make: "NIKON CORPORATION", model: "NIKON D5100", size: (4992, 3280), crop: (8, 8, 4928, 3264)),
    ImageArea(make: "NIKON CORPORATION", model: "NIKON D800E", size: (7424, 4924), crop: (8, 6, 7360, 4912)),
    ImageArea(make: "SONY", model: "ILCE-7", size: (6048, 4024), crop: (12, 12, 6000, 4000))]

proc applyCameraProfile*(s: var Sensor) =
  ## Applies the measured profile of the camera that made `s`, when there is one, and its
  ## measured image area when the profile is not measured yet.
  if s.make.isNone or s.model.isNone: return
  let (make, model) = (s.make.get.strip, s.model.get.strip)
  for a in ImageAreas:
    if a.make == make and a.model == model and a.size == (s.width, s.height) and s.crop == s.active:
      s.crop = rawRect(a.crop[0], a.crop[1], a.crop[2], a.crop[3])
      break
  for p in Profiles:
    if make == p.make and model == p.model and (s.width, s.height) == p.size:
      s.crop = rawRect(p.crop[0], p.crop[1], p.crop[2], p.crop[3])
      s.white = [p.white, p.white, p.white]
      if p.blackZero:
        var kept: seq[string]
        for w in s.warnings:
          if "black level not recorded" notin w: kept.add w
        s.warnings = kept
      # D50 daylight: the matrices are consistent at the camera's daylight neutral.
      s.color = ColorInfo(calibrations: @[Calibration(temperature: 5003, colorMatrix: D4Color,
                                                      forwardMatrix: some(D4Forward), cameraCalibration: Identity)])
      s.toneCurve = D4Tone
      return
