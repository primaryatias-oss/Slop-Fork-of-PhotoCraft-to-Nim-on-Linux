## Named character/paragraph styles ↔ the style sheets inside text layers' `TySh` engine data.
##
## Deviation: until the type engine's PSD style-sheet reader is ported, named styles are not
## imported from PSD (text layers keep their per-run styles) and exported `TySh` data is kept
## as read.

import std/options
import ../doc

proc importStyles*(d: var Document): seq[string] =
  ## Named text styles found in the document's text layers (none yet; see the module note).
  @[]

proc exportTysh*(data: openArray[byte]; t: TextLayer; styles: TextStyles; dpi: float32): Option[seq[byte]] =
  none(seq[byte])
