## Spelling, UI fallback scripts and the Glyphs panel (ports of the Rust `spell`, `scripts` and
## `glyphs` tests; suggestion lists checked against the Rust oracle).

import std/[unittest, options, sets, os, unicode]
import photocraft/[doc, text]
import photocraft/text/[spell, scripts, glyphs, fonts]

suite "spelling":
  let d = english()
  let none0 = initHashSet[string]()
  test "dictionary loads":
    check d.len > 70_000
  test "case rules":
    check d.check("the", none0) and d.check("The", none0) and d.check("THE", none0)
    check not d.check("paris", none0) and d.check("Paris", none0) and d.check("PARIS", none0)
    check d.check("NASA", none0) and not d.check("nasa", none0)
    check d.check("dog's", none0) and d.check("x2", none0) and d.check("don’t", none0)
    check d.check("teh", ["teh"].toHashSet)
  test "suggestions match Rust":
    check d.suggest("brwon", 5) == @["brown", "Brown", "Bryon", "grown", "won"]
    check d.suggest("teh", 5) == @["the", "tea", "ten", "eh", "tee"]
    check d.suggest("Recieve", 5) == @["Receive", "Relieve", "Believe", "Received", "Receives"]
    check d.suggest("nasa", 5) == @["NASA", "nasal", "Napa", "Nash", "NSA"]
  test "misspellings and words":
    let m = d.misspellings("Teh quick brwon fox jumpd over the lazy dog's back. It's 2024.", none0)
    check m.len == 3 and m[0].word == "Teh" and m[1].start == 10 and m[2].`end` == 25
    check words("don't stop-me now 'quoted' ok") == @[(0, 5), (6, 10), (11, 13), (14, 17), (19, 25), (27, 29)]
    check words("") .len == 0

suite "fallback scripts":
  test "classifies scripts and leaves Latin and CJK alone":
    for (c, s) in [("ส", fsThai), ("ม", fsThai), ("ر", fsArabic), ("ﻻ", fsArabic), ("ש", fsHebrew),
                   ("न", fsDevanagari), ("த", fsTamil), ("ក", fsKhmer), ("ა", fsGeorgian)]:
      check classify(int(c.runeAt(0))) == some(s)
    for c in ["A", "é", "Ж", "Ω", "–", "日", "カ", "한", " "]:
      check classify(int(c.runeAt(0))).isNone
  test "candidates match file names, regular first, across layouts":
    let files = @["/usr/share/fonts/google-noto/NotoSansThai-Bold.ttf",
      "/usr/share/fonts/google-noto-vf/NotoSansThai[wght].ttf", "/usr/share/fonts/truetype/noto/NotoSansThai-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSansThaiLooped-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSansThaana-Regular.ttf", "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
      "/usr/share/fonts/truetype/lohit-devanagari/Lohit-Devanagari.ttf", "/usr/share/fonts/Lohit-Tamil.ttf",
      "/mnt/c/Windows/Fonts/LeelawUI.ttf", "/mnt/c/Windows/Fonts/Nirmala.ttc",
      "/System/Library/Fonts/Supplemental/Thonburi.ttc", "/System/Library/Fonts/Supplemental/Kohinoor.ttc",
      "/usr/share/fonts/README.txt"]
    proc names(s: FallbackScript): seq[string] =
      for p in candidates(s, files): result.add extractFilename(p)
    check names(fsThai) == @["NotoSansThai-Regular.ttf", "NotoSansThai[wght].ttf", "NotoSansThai-Bold.ttf",
                            "NotoSansThaiLooped-Regular.ttf", "LeelawUI.ttf", "Thonburi.ttc"]
    check names(fsArabic) == @["DejaVuSans.ttf", "DejaVuSans-Bold.ttf"]
    check names(fsDevanagari) == @["Nirmala.ttc", "Kohinoor.ttc", "Lohit-Devanagari.ttf"]
    check names(fsTamil) == @["Nirmala.ttc", "Lohit-Tamil.ttf"]
    check names(fsEthiopic).len == 0
    check candidates(fsThai, newSeq[string]()).len == 0
  test "covering face checks the character map and rejects garbage":
    let inter = bundledFont("Inter-Regular.ttf")
    check coveringFace(inter, ord('A')) == some(0)
    check coveringFace(inter, 0x0E2A).isNone
    for bad in ["", "ttcf", "ttcf\0\x01\0\0\0\0\0\x02\xff\xff\xff\xff\0\0\0\x10", "\0\x01\0\0\0\xff"]:
      check coveringFace(cast[seq[byte]](bad), ord('A')).isNone

suite "glyphs panel":
  test "charmap, categories and preview match Rust":
    let e = newTextEngine()
    let cm = e.fonts.charmap("JetBrains Mono", 400, false)
    var sum = 0
    for c in cm: sum += c
    check cm.len == 1369 and sum == 12951175 and cm[0] == 32
    var basic = 0
    for c in cm:
      if inCategory(c, "Basic Latin"): inc basic
    check basic == 95
    check e.fonts.charmap("No Such Family", 400, false).len == 0
    var st = defaultCharStyle()
    st.fontFamily = "JetBrains Mono"
    let (px, a) = preview(e, st, ord('@'), 32)
    var s, nz = 0
    for v in a:
      s += int(v)
      if v > 0: inc nz
    check px == 32 and s == 27178 and nz == 165
