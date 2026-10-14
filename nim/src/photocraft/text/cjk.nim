## CJK font fallback: which installed fonts cover Japanese, Simplified Chinese, Traditional
## Chinese and Korean, and in which order to try them for the user's locale (port of
## `photocraft_text::cjk`). Han is shared between the four but its preferred forms differ, so the
## locale's script goes first; kana prefers Japanese fonts, Hangul Korean, Bopomofo Traditional.

import std/[os, strutils, options]

type
  CjkScript* = enum Japanese, SimplifiedChinese, TraditionalChinese, Korean
  CjkChar* = enum ccHan, ccKana, ccHangul, ccBopomofo
  FontFile* = object
    path*: string
    family*: string   ## family of the wanted face in a collection ("" = face 0)

proc preferred*(c: CjkChar; order: array[4, CjkScript]): CjkScript =
  case c
  of ccKana: Japanese
  of ccHangul: Korean
  of ccBopomofo: TraditionalChinese
  of ccHan: order[0]

proc classify*(u: int): Option[CjkChar] =
  ## The kind of CJK character `u` is, none for everything else.
  if u in 0x1100 .. 0x11FF or u in 0x3130 .. 0x318F or u in 0xA960 .. 0xA97F or u in 0xAC00 .. 0xD7AF or
     u in 0xD7B0 .. 0xD7FF or u in 0xFFA0 .. 0xFFDC: return some(ccHangul)
  if u in 0x3040 .. 0x30FF or u in 0x31F0 .. 0x31FF or u in 0xFF65 .. 0xFF9F or u in 0x1B000 .. 0x1B16F:
    return some(ccKana)
  if u in 0x3100 .. 0x312F or u in 0x31A0 .. 0x31BF: return some(ccBopomofo)
  if u in 0x2E80 .. 0x2FDF or u in 0x3000 .. 0x303F or u in 0x3190 .. 0x319F or u in 0x31C0 .. 0x31EF or
     u in 0x3200 .. 0x33FF or u in 0x3400 .. 0x4DBF or u in 0x4E00 .. 0x9FFF or u in 0xF900 .. 0xFAFF or
     u in 0xFE30 .. 0xFE4F or u in 0xFF00 .. 0xFF64 or u in 0xFFE0 .. 0xFFEF or u in 0x20000 .. 0x3FFFF:
    return some(ccHan)
  none(CjkChar)

proc scriptOrder*(locale: string): array[4, CjkScript] =
  ## Script order for a locale tag (`ja_JP.UTF-8`, `zh-Hant-TW`, `ko-KR`, …). Non-CJK locales put
  ## Simplified Chinese first, so Japanese forms only win for a Japanese locale.
  var tag = locale
  for sep in ['.', '@']:
    let i = tag.find(sep)
    if i >= 0: tag = tag[0 ..< i]
  tag = tag.toLowerAscii.replace('_', '-')
  let parts = tag.split('-')
  let lang = parts[0]
  let rest = if parts.len > 1: parts[1 .. ^1] else: @[]
  case lang
  of "ja": [Japanese, SimplifiedChinese, TraditionalChinese, Korean]
  of "ko": [Korean, TraditionalChinese, SimplifiedChinese, Japanese]
  of "zh", "yue":
    var hant, hans = false
    for p in rest:
      if p in ["hant", "tw", "hk", "mo"]: hant = true
      if p in ["hans", "cn", "sg"]: hans = true
    if (hant or lang == "yue") and not hans: [TraditionalChinese, SimplifiedChinese, Japanese, Korean]
    else: [SimplifiedChinese, TraditionalChinese, Japanese, Korean]
  else: [SimplifiedChinese, TraditionalChinese, Japanese, Korean]

var cachedLocale: Option[string]
var localeDetected = false

proc detectLocale(): Option[string] =
  proc env(k: string): string =
    let v = getEnv(k).strip
    if v.len == 0 or v == "C" or v == "POSIX" or v.startsWith("C."): "" else: v
  let o = env("PHOTOCRAFT_LOCALE")
  if o.len > 0: return some(o)
  for k in ["LC_ALL", "LC_MESSAGES", "LANG", "LANGUAGE"]:
    let v = env(k)
    if v.len > 0: return some(v.split(':')[0])
  none(string)

proc uiLocale*(): Option[string] =
  ## The user's UI locale: `PHOTOCRAFT_LOCALE`, else `LC_ALL` / `LC_MESSAGES` / `LANG` / `LANGUAGE`.
  if not localeDetected:
    cachedLocale = detectLocale()
    localeDetected = true
  cachedLocale

proc uiScriptOrder*(): array[4, CjkScript] = scriptOrder(uiLocale().get(""))

proc families*(s: CjkScript): seq[string] =
  ## Family names per script, most preferred first.
  case s
  of Japanese: @["Hiragino Sans", "Hiragino Kaku Gothic ProN", "Yu Gothic", "Meiryo", "Noto Sans CJK JP",
                 "Noto Sans JP", "IPAexGothic"]
  of SimplifiedChinese: @["PingFang SC", "Hiragino Sans GB", "Microsoft YaHei", "Noto Sans CJK SC",
                          "Noto Sans SC", "WenQuanYi Micro Hei", "WenQuanYi Zen Hei", "Heiti SC"]
  of TraditionalChinese: @["PingFang TC", "Microsoft JhengHei", "Noto Sans CJK TC", "Noto Sans TC",
                           "Hiragino Sans CNS", "Heiti TC"]
  of Korean: @["Apple SD Gothic Neo", "Malgun Gothic", "Noto Sans CJK KR", "Noto Sans KR", "NanumGothic",
               "UnDotum"]

proc withFlatpakHostFonts(files: seq[FontFile]): seq[FontFile] =
  for f in files:
    result.add f
    when defined(linux):
      if f.path.startsWith("/usr/share/fonts"):
        result.add FontFile(path: "/run/host/fonts" & f.path["/usr/share/fonts".len .. ^1], family: f.family)

proc fontFiles*(s: CjkScript): seq[FontFile] =
  ## System font files for `s`, most preferred first (paths that don't exist are included).
  when defined(macosx):
    case s
    of Japanese: @[FontFile(path: "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc"),
                   FontFile(path: "/System/Library/Fonts/ヒラギノ角ゴシック W4.ttc")]
    of SimplifiedChinese: @[FontFile(path: "/System/Library/Fonts/PingFang.ttc", family: "PingFang SC"),
                            FontFile(path: "/System/Library/Fonts/Hiragino Sans GB.ttc"),
                            FontFile(path: "/System/Library/Fonts/STHeiti Light.ttc", family: "Heiti SC")]
    of TraditionalChinese: @[FontFile(path: "/System/Library/Fonts/PingFang.ttc", family: "PingFang TC"),
                             FontFile(path: "/System/Library/Fonts/STHeiti Light.ttc", family: "Heiti TC")]
    of Korean: @[FontFile(path: "/System/Library/Fonts/AppleSDGothicNeo.ttc", family: "Apple SD Gothic Neo")]
  elif defined(windows):
    let dir = getEnv("WINDIR", "C:\\Windows") / "Fonts"
    case s
    of Japanese: @[FontFile(path: dir / "YuGothR.ttc", family: "Yu Gothic"), FontFile(path: dir / "YuGothM.ttc", family: "Yu Gothic"),
                   FontFile(path: dir / "meiryo.ttc", family: "Meiryo"), FontFile(path: dir / "msgothic.ttc")]
    of SimplifiedChinese: @[FontFile(path: dir / "msyh.ttc", family: "Microsoft YaHei"), FontFile(path: dir / "msyh.ttf"),
                            FontFile(path: dir / "simsun.ttc")]
    of TraditionalChinese: @[FontFile(path: dir / "msjh.ttc", family: "Microsoft JhengHei"), FontFile(path: dir / "msjh.ttf"),
                             FontFile(path: dir / "mingliu.ttc")]
    of Korean: @[FontFile(path: dir / "malgun.ttf"), FontFile(path: dir / "gulim.ttc")]
  else:
    let noto = case s
      of Japanese: "Noto Sans CJK JP"
      of SimplifiedChinese: "Noto Sans CJK SC"
      of TraditionalChinese: "Noto Sans CJK TC"
      of Korean: "Noto Sans CJK KR"
    var v: seq[FontFile]
    for p in ["/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc",
              "/usr/local/share/fonts/noto/NotoSansCJK-Regular.ttc",
              "/usr/local/share/fonts/noto-cjk/NotoSansCJK-Regular.ttc",
              "/usr/local/share/fonts/noto/NotoSansCJK-VF.ttc",
              "/usr/share/fonts/noto-cjk/NotoSansCJK-Regular.ttc",
              "/usr/share/fonts/truetype/noto/NotoSansCJK-Regular.ttc",
              "/usr/share/fonts/google-noto-cjk/NotoSansCJK-Regular.ttc",
              "/usr/share/fonts/google-noto-sans-cjk-fonts/NotoSansCJK-Regular.ttc",
              "/usr/share/fonts/google-noto-sans-cjk-vf-fonts/NotoSansCJK-VF.ttc",
              "/usr/share/fonts/noto-cjk/NotoSansCJK-VF.ttc"]:
      v.add FontFile(path: p, family: noto)
    case s
    of Japanese:
      for p in ["/usr/share/fonts/opentype/ipafont-gothic/ipag.ttf", "/usr/share/fonts/truetype/fonts-japanese-gothic.ttf",
                "/usr/share/fonts/truetype/takao-gothic/TakaoPGothic.ttf"]: v.add FontFile(path: p)
    of SimplifiedChinese, TraditionalChinese:
      for p in ["/usr/share/fonts/truetype/wqy/wqy-microhei.ttc", "/usr/share/fonts/wenquanyi/wqy-microhei/wqy-microhei.ttc",
                "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc", "/usr/share/fonts/wenquanyi/wqy-zenhei/wqy-zenhei.ttc"]:
        v.add FontFile(path: p)
    of Korean:
      for p in ["/usr/share/fonts/truetype/nanum/NanumGothic.ttf", "/usr/share/fonts/nanum/NanumGothic.ttf",
                "/usr/share/fonts/truetype/unfonts-core/UnDotum.ttf", "/usr/share/fonts/un-core/UnDotum.ttf"]:
        v.add FontFile(path: p)
    withFlatpakHostFonts(v)

proc lastResortFiles*(): seq[FontFile] =
  ## Broad-coverage fonts tried after every script's own fonts.
  when defined(macosx):
    @[FontFile(path: "/System/Library/Fonts/Supplemental/Arial Unicode.ttf"), FontFile(path: "/Library/Fonts/Arial Unicode.ttf")]
  elif defined(windows):
    @[FontFile(path: getEnv("WINDIR", "C:\\Windows") / "Fonts" / "ARIALUNI.TTF")]
  else:
    withFlatpakHostFonts(@[FontFile(path: "/usr/share/fonts/truetype/droid/DroidSansFallbackFull.ttf"),
                           FontFile(path: "/usr/share/fonts/google-droid-sans-fonts/DroidSansFallbackFull.ttf")])
