## WebP (placeholder; replaced by the full VP8/VP8L implementation).
import ./image
proc decodeWebp*(b: openArray[byte]; limits: Limits): Image = raise unsupported(fmtWebP, "not implemented yet")
proc encodeWebp*(img: Image; layout: ChannelLayout; opts: EncodeOptions): seq[byte] = raise unsupported(fmtWebP, "not implemented yet")
