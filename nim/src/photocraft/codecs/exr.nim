## OpenEXR (placeholder; replaced by the full implementation).
import ./image
proc decodeExr*(b: openArray[byte]; limits: Limits): Image = raise unsupported(fmtOpenExr, "not implemented yet")
proc encodeExr*(img: Image; layout: ChannelLayout; sample: ImgSample; opts: EncodeOptions): seq[byte] = raise unsupported(fmtOpenExr, "not implemented yet")
