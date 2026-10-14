## Fonts from the optional craft-fonts build input (https://github.com/storytold/craft-fonts).
## The Rust build embeds them when built with `CRAFT_FONTS_DIR`; this port doesn't embed them yet,
## so `CraftFonts` is empty and every user works without them (as the Rust build must too).

import std/[strutils, algorithm]

type CraftFont* = object
  family*, style*: string
  scripts*: seq[string]   ## ISO 15924, e.g. "Jpan"
  bytes*: seq[byte]

const UiJapaneseFamily* = "BIZ UDPGothic"

var craftFontList*: seq[CraftFont]

proc isJapanese*(f: CraftFont): bool = "Jpan" in f.scripts
proc isMincho*(f: CraftFont): bool = "Mincho" in f.family

proc japaneseFamilies*(): seq[string] =
  ## Japanese craft families for document fallback: Gothic first, then Mincho.
  var v: seq[CraftFont]
  for f in craftFontList:
    if f.isJapanese: v.add f
  v.sort(proc(a, b: CraftFont): int =
    cmp((a.family != UiJapaneseFamily, a.style != "Regular"), (b.family != UiJapaneseFamily, b.style != "Regular")))
  for f in v:
    if f.family notin result: result.add f.family
  result.sort(proc(a, b: string): int = cmp("Mincho" in a, "Mincho" in b))

proc minchoFirst*(fallback: seq[string]): seq[string] =
  ## `fallback` with the craft Mincho families moved ahead of the craft Gothic one (serif runs).
  let craft = japaneseFamilies()
  var slots: seq[int]
  for i, f in fallback:
    if f in craft: slots.add i
  var group: seq[string]
  for i in slots: group.add fallback[i]
  group.sort(proc(a, b: string): int = cmp("Mincho" notin a, "Mincho" notin b))
  result = fallback
  for k, slot in slots: result[slot] = group[k]

proc isSerifFamily*(name: string): bool =
  ## A family name that reads as serif: its Japanese fallback should be a Mincho face.
  let n = name.toLowerAscii
  if "sans" in n or "gothic" in n: return false
  for s in ["serif", "mincho", "minchō", "song", "ming", "myeongjo", "myungjo", "batang", "times", "georgia",
            "garamond", "minion", "baskerville", "caslon", "bodoni", "didot", "palatino", "cambria",
            "book antiqua", "century", "hoefler"]:
    if s in n: return true
  false
