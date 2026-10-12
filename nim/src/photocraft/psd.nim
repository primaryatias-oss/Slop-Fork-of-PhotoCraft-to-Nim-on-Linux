## photocraft/psd: faithful, lossless reader and writer for Adobe Photoshop PSD (version 1) and
## PSB (version 2) files, implemented clean-room from Adobe's public file format specification,
## plus the related Photoshop file formats (brushes `.abr`, swatches `.aco`/`.ase`, gradients
## `.grd`, patterns `.pat`, contours `.shc`) and layered-TIFF layer data.
##
## * `parsePsd` parses a file into a format-level model; `toBytes` writes it back. Unmodified
##   files round-trip byte for byte.
## * Channel data is decoded lazily with all four compression methods.
## * `PsdBuilder` constructs new files from RGBA/gray/CMYK buffers (the caller supplies the
##   merged composite; this module never composites).
## * The parser never crashes on malformed input: it raises `PsdError` and enforces limits.

import psd/[io, compression, descriptor, model, builder, path, hdr, swatches, grd, slices, patterns,
            filter_effects, abr, tiff]
export io, compression, descriptor, model, builder, path, hdr, swatches, grd, slices, patterns,
       filter_effects, abr, tiff
