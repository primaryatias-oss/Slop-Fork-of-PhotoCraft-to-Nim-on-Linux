## PhotoCraft colour management: a pure-Nim ICC v2/v4 colour management module.
##
## * `Profile` parses ICC profiles (matrix/TRC RGB, gray TRC, `mft1`/`mft2`/`mAB `/`mBA `
##   LUT profiles incl. CMYK↔Lab, `curv`/`para` curves) and writes ICC v4 profiles.
## * `Transform` connects two profiles through the PCS (XYZ/Lab D50) with a rendering
##   `Intent` and optional black point compensation, and converts 8/16-bit and float pixel
##   buffers through precomputed tables (tetrahedral 3D/4D interpolation).
## * `builtin` ships sRGB, Display P3, Adobe RGB (1998)-compatible, ProPhoto-compatible,
##   linear sRGB, Rec. 2020, Gray 2.2, sGray, Lab D50 and a synthetic coated CMYK profile.
##
## Implemented from the public ICC.1:2010 (v4.3) and ICC.1:2001-04 (v2) specifications,
## CIE 15, and Adobe's published black point compensation paper. Depends on no other
## PhotoCraft module. Port of `crates/cms`.

import cms/[math, curve, clut, pipeline, profile, write, synth, builtin, transform, gamut, lut3d, lutfile]
export math, curve, clut, pipeline, profile, write, synth, builtin, transform, gamut, lut3d, lutfile
