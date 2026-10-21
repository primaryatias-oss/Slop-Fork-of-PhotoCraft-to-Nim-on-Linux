## Colour management commands and helpers (port of `color_cmds.rs`, with the monitor-profile
## half of `display_color.rs`): document profiles (Edit › Assign / Convert to Profile), CMS-based
## Image › Mode conversions, Color Settings, soft proofing (Proof Setup / Proof Colors / Gamut
## Warning). The canvas display transforms and LUTs land with the UI. Part of the `engine/core`
## unit: included, not imported.

# ---------------------------------------------------------------- settings

proc policyParse*(s: string): Option[Policy] =
  case s
  of "preserve", "preserveEmbedded": some(poPreserve)
  of "convert", "convertToWorking": some(poConvert)
  of "off": some(poOff)
  else: none(Policy)

proc defaultColorSettings*(): ColorSettings =
  ColorSettings(workingRgb: bSrgb.id, workingCmyk: bCoatedCmyk.id, workingGray: bSGray.id,
                policyRgb: poPreserve, policyCmyk: poPreserve, policyGray: poPreserve,
                askOnMismatch: true, askOnPaste: true, askOnMissing: false,
                intent: RelativeColorimetric.id, bpc: true, dither: true, blendTextGamma: TextGammaDefault,
                monitorProfile: "auto")

proc settingsIntent*(c: ColorSettings): Intent =
  let i = parseIntent(c.intent)
  if i.isSome: i.get else: RelativeColorimetric

proc colorSettingsIntent*(s: Session): Intent = s.color.settings.settingsIntent

proc modeSpace*(mode: ColorMode): ColorSpace =
  ## ICC colour space of a document mode.
  case mode
  of cmGrayscale, cmBitmap, cmDuotone: ColorSpace(kind: csGray)
  of cmCmyk: ColorSpace(kind: csCmyk)
  of cmLab: ColorSpace(kind: csLab)
  else: ColorSpace(kind: csRgb)

proc spaceMode(cs: ColorSpace): Option[ColorMode] =
  case cs.kind
  of csRgb: some(cmRgb)
  of csGray: some(cmGrayscale)
  of csCmyk: some(cmCmyk)
  of csLab: some(cmLab)
  else: none(ColorMode)

proc policyFor*(c: ColorSettings; mode: ColorMode): Policy =
  case modeSpace(mode).kind
  of csCmyk: c.policyCmyk
  of csGray: c.policyGray
  of csRgb: c.policyRgb
  else: poPreserve

proc workingSpec(c: ColorSettings; space: ColorSpace): Option[string] =
  case space.kind
  of csRgb: some(c.workingRgb)
  of csCmyk: some(c.workingCmyk)
  of csGray: some(c.workingGray)
  else: none(string)

proc csDebug*(cs: ColorSpace): string =
  ## Rust's `{:?}` of a colour space.
  case cs.kind
  of csXyz: "Xyz"
  of csLab: "Lab"
  of csLuv: "Luv"
  of csYCbCr: "YCbCr"
  of csYxy: "Yxy"
  of csRgb: "Rgb"
  of csGray: "Gray"
  of csHsv: "Hsv"
  of csHls: "Hls"
  of csCmyk: "Cmyk"
  of csCmy: "Cmy"
  of csColor: "Color(" & $cs.n & ")"
  of csUnknown: "Unknown(" & $cs.raw & ")"

proc classDebug(c: ProfileClass): string =
  case c.kind
  of pcInput: "Input"
  of pcDisplay: "Display"
  of pcOutput: "Output"
  of pcDeviceLink: "DeviceLink"
  of pcColorSpace: "ColorSpace"
  of pcAbstract: "Abstract"
  of pcNamedColor: "NamedColor"
  of pcUnknown: "Unknown(" & $c.raw & ")"

proc cmsErr(e: ref CatchableError): ref EngineError = other("colour management: " & e.msg)

# ---------------------------------------------------------------- profiles

proc workingProfile*(mode: ColorMode): Profile =
  ## Working (default) profile of a mode.
  let p = defaultFor(modeSpace(mode))
  if p.isSome: p.get else: bSrgb.profile

var profileCache {.threadvar.}: seq[(SharedBytes, Profile)]

proc profileFromBytes*(bytes: SharedBytes): Profile =
  ## Parses ICC bytes, cached by the shared allocation. Raises `CmsError`.
  for (b, p) in profileCache:
    if cast[pointer](b) == cast[pointer](bytes): return p
  result = parseProfile(bytes.data)
  if profileCache.len >= 32: profileCache.delete(0)
  profileCache.add (bytes, result)

proc embeddedProfile(doc: Document): Option[Profile] =
  if doc.iccProfile.isNil: return
  try:
    let p = profileFromBytes(doc.iccProfile)
    if p.colorSpace == modeSpace(doc.mode): some(p) else: none(Profile)
  except CatchableError: none(Profile)

proc documentProfile*(doc: Document): Profile =
  ## The document's profile: its embedded profile when it parses and matches the mode,
  ## otherwise the mode's working profile.
  let e = embeddedProfile(doc)
  if e.isSome: e.get else: workingProfile(doc.mode)

proc compositeProfile*(doc: Document): Profile =
  ## Profile describing the compositor's RGB output for this document.
  case modeSpace(doc.mode).kind
  of csRgb: documentProfile(doc)
  of csGray:
    let p = documentProfile(doc)
    let a = p.grayAsRgb
    if a.isSome: return a.get
    let b = bSGray.profile.grayAsRgb
    if b.isSome: b.get else: bSrgb.profile
  else: bSrgb.profile

proc resolveProfile*(spec: string; doc: Option[Document]; mode: Option[ColorMode]): Profile =
  ## Resolves a profile parameter: a built-in id/alias, "working" (with `mode`), "document",
  ## or a path to an `.icc`/`.icm` file.
  case spec
  of "working", "default":
    let m = if mode.isSome: mode.get elif doc.isSome: doc.get.mode else: cmRgb
    return workingProfile(m)
  of "working-cmyk", "workingCmyk": return workingProfile(cmCmyk)
  of "working-rgb", "workingRgb": return workingProfile(cmRgb)
  of "working-gray", "workingGray": return workingProfile(cmGrayscale)
  of "document":
    if doc.isNone: raise noDocument()
    return documentProfile(doc.get)
  else: discard
  let b = builtinFromId(spec)
  if b.isSome: return b.get.profile
  let lower = spec.toLowerAscii
  if lower.endsWith(".icc") or lower.endsWith(".icm") or '/' in spec or '\\' in spec:
    var bytes: seq[byte]
    try: bytes = readBytesRust(spec)
    except OSError as e: raise other("cannot read profile `" & spec & "`: " & e.msg)
    try: return parseProfile(bytes)
    except CatchableError as e: raise cmsErr(e)
  var ids: seq[string]
  for x in Builtin: ids.add x.id
  raise other("unknown profile `" & spec & "` (built-ins: " & ids.join(", ") & ", or a path to an .icc file)")

proc working*(c: ColorState; mode: ColorMode): Profile =
  ## The working profile of a mode from Color Settings (the built-in when it does not resolve).
  let space = modeSpace(mode)
  let spec = c.settings.workingSpec(space)
  if spec.isSome:
    try:
      let p = resolveProfile(spec.get, none(Document), some(mode))
      if p.colorSpace == space: return p
    except CatchableError: discard
  workingProfile(mode)

proc resolve*(c: ColorState; spec: string; doc: Option[Document]; mode: Option[ColorMode]): Profile =
  ## Resolves a profile spec, reading "working…" specs from Color Settings.
  case spec
  of "working", "default":
    c.working(if mode.isSome: mode.get elif doc.isSome: doc.get.mode else: cmRgb)
  of "working-cmyk", "workingCmyk": c.working(cmCmyk)
  of "working-rgb", "workingRgb": c.working(cmRgb)
  of "working-gray", "workingGray": c.working(cmGrayscale)
  else: resolveProfile(spec, doc, mode)

proc validateSettings*(c: ColorSettings): string =
  ## Checks that each working space resolves to a profile of the right colour space ("" = ok).
  for (space, spec) in [(ColorSpace(kind: csRgb), c.workingRgb), (ColorSpace(kind: csCmyk), c.workingCmyk),
                        (ColorSpace(kind: csGray), c.workingGray)]:
    var p: Profile
    try: p = resolveProfile(spec, none(Document), spaceMode(space))
    except EngineError as e: return e.msg
    if p.colorSpace != space: return "working space `" & spec & "` is " & csDebug(p.colorSpace) & ", not " & csDebug(space)
  let m = c.monitorProfile
  if not (m.len == 0 or m == "auto"):
    var p: Profile
    try: p = resolveProfile(m, none(Document), some(cmRgb))
    except EngineError as e: return e.msg
    if p.colorSpace.kind != csRgb: return "monitor profile `" & m & "` is " & csDebug(p.colorSpace) & ", not RGB"
  if parseIntent(c.intent).isNone:
    return "unknown intent `" & c.intent & "` (perceptual|relative|saturation|absolute)"
  ""

proc defaultProofView(): ProofView =
  ProofView(setup: ProofSetup(name: "working-cmyk", profile: bCoatedCmyk.profile, intent: RelativeColorimetric, bpc: true),
            gamutThreshold: DefaultGamutThreshold)

proc proof*(c: ColorState; doc: DocId): ProofView =
  ## Proofing state of a document (defaults: coated CMYK, relative colorimetric + BPC, off).
  c.proofs.getOrDefault(uint64(doc), defaultProofView())

proc proofMut*(c: var ColorState; doc: DocId): var ProofView =
  if uint64(doc) notin c.proofs: c.proofs[uint64(doc)] = defaultProofView()
  c.proofs[uint64(doc)]

proc mergeJson(a, b: JsonNode): JsonNode =
  result = a
  if a.isObject and b.isObject:
    for k, v in b.fields: result[k] = v

proc openPolicyApply(c: ColorState; doc: var Document; space: ColorSpace): JsonNode

proc openPolicy*(c: ColorState; doc: var Document; ignoreExif: bool): JsonNode =
  ## Applies the colour management policy to a document being opened.
  let space = modeSpace(doc.mode)
  if space.kind notin {csRgb, csCmyk, csGray}: return %*{"action": "kept"}
  var exifSrgb = false
  if space.kind == csRgb and not ignoreExif and doc.iccProfile.isNil and not doc.metadata.exif.isNil:
    let cs = exifColorSpace(doc.metadata.exif.data)
    exifSrgb = cs.isSome and cs.get == ExifColorSpaceSrgb
  if exifSrgb: doc.iccProfile = shared(bSrgb.profile.toBytes)
  let report = c.openPolicyApply(doc, space)
  if exifSrgb: mergeJson(report, %*{"exif": true}) else: report

proc convertDocument*(doc: var Document; dst: Profile; intent: Intent; bpc: bool)

proc openPolicyApply(c: ColorState; doc: var Document; space: ColorSpace): JsonNode =
  let working = c.working(doc.mode)
  let policy = c.settings.policyFor(doc.mode)
  var embedded = none(Profile)
  if not doc.iccProfile.isNil:
    try:
      let p = profileFromBytes(doc.iccProfile)
      if p.colorSpace == space: embedded = some(p)
    except CatchableError: discard
  if embedded.isSome:
    let emb = embedded.get
    # Compared by colour, not bytes.
    let mismatch = not emb.sameColors(working)
    let base = %*{"embedded": emb.description, "working": working.description, "mismatch": mismatch, "policy": $policy}
    # 32-bit linear images stay linear.
    let linearHdr = doc.depth == stF32 and (emb.sameColors(bLinearSrgb.profile) or emb.sameColors(linearGray()))
    if not mismatch or linearHdr: return mergeJson(base, %*{"action": "kept"})
    var action = "kept"
    case policy
    of poPreserve: action = "kept"
    of poOff:
      doc.iccProfile = nil
      action = "discarded"
    of poConvert:
      try:
        convertDocument(doc, working, c.settings.settingsIntent, c.settings.bpc)
        action = "converted"
      except CatchableError: action = "kept"
    return mergeJson(base, %*{"action": action, "ask": c.settings.askOnMismatch})
  # Untagged files are assumed to be in the working space; tag them when it is not the
  # built-in default so they display (and save) as intended.
  let default = workingProfile(doc.mode)
  let assign = policy != poOff and working.contentHash != default.contentHash
  if assign: doc.iccProfile = shared(working.toBytes)
  %*{"action": (if assign: "assigned" else: "untagged"), "working": working.description, "policy": $policy,
     "ask": c.settings.askOnMissing, "missing": true}

# ---------------------------------------------------------------- monitor (display_color.rs)

const
  DisplayIntent* = RelativeColorimetric
  DisplayBpc* = true

proc detectionJson(d: MonitorDetection): JsonNode =
  case d.kind
  of mdUnsupported: %*{"state": "unsupported"}
  of mdPending: %*{"state": "pending"}
  of mdFailed: %*{"state": "failed", "reason": d.reason}
  of mdFound: %*{"state": "found"}
  of mdRetained: %*{"state": "retained", "reason": d.reason}

proc displayOrPrimary(c: ColorState; id: Option[uint32]): ptr Display =
  if id.isSome:
    for i in 0 ..< c.displays.len:
      if c.displays[i].id == id.get: return unsafeAddr c.displays[i]
  if c.displays.len > 0: unsafeAddr c.displays[0] else: nil

proc resolvedMonitor(c: ColorState; display: Option[uint32]): (Profile, JsonNode) =
  ## The monitor profile for `display` and its status (`MonitorStatus` as JSON).
  let spec = c.settings.monitorProfile
  let auto = spec.len == 0 or spec == "auto"
  let shown = c.displayOrPrimary(display)
  var found: Profile
  var err = ""
  if auto:
    if not shown.isNil and not shown.icc.isNil:
      try: found = profileFromBytes(shown.icc)
      except CatchableError as e: err = "the display profile can't be read: " & e.msg
    elif not shown.isNil: err = shown.name & " has no ICC profile"
    elif c.monitorDetection.kind == mdPending: err = "the display profile hasn't been read yet"
    elif c.monitorDetection.kind == mdFailed: err = c.monitorDetection.reason
    else: err = "this platform doesn't report display profiles"
  else:
    try: found = resolveProfile(spec, none(Document), some(cmRgb))
    except EngineError as e: err = e.msg
  if err.len == 0:
    if found.colorSpace.kind != csRgb:
      err = "`" & found.description & "` is a " & csDebug(found.colorSpace) & " profile, not RGB"
    else:
      try: discard newTransform(bSrgb.profile, found, DisplayIntent, DisplayBpc)
      except CatchableError as e: err = "`" & found.description & "` can't be used as a display profile: " & e.msg
  var source = if auto: "auto" else: "manual"
  var reason = newJNull()
  if err.len > 0:
    found = bSrgb.profile
    source = "fallback"
    reason = %err
  let status = %*{
    "requested": (if spec.len == 0: "auto" else: spec), "source": source,
    "display": (if shown.isNil: newJNull() else: %shown.name), "profile": found.description,
    "profileName": (if auto and not shown.isNil and shown.profileName.isSome: %shown.profileName.get else: newJNull()),
    "fingerprint": toHex(found.contentHash, 16).toLowerAscii, "detection": detectionJson(c.monitorDetection),
    "reason": reason}
  (found, status)

proc monitorFor*(c: ColorState; display: Option[uint32]): Profile = c.resolvedMonitor(display)[0]
proc monitor*(c: ColorState): Profile = c.monitorFor(c.mainDisplay)
proc monitorStatusFor*(c: ColorState; display: Option[uint32]): JsonNode = c.resolvedMonitor(display)[1]
proc monitorStatus*(c: ColorState): JsonNode = c.monitorStatusFor(c.mainDisplay)

proc displayStatuses*(c: ColorState): JsonNode =
  result = newJArray()
  for d in c.displays:
    result.add %*{"id": d.id, "name": d.name, "frame": d.frame,
                  "profileName": (if d.profileName.isSome: %d.profileName.get else: newJNull()),
                  "main": c.mainDisplay == some(d.id), "status": c.monitorStatusFor(some(d.id))}

proc setDisplays*(c: var ColorState; displays: seq[Display]; error = ""): Option[string] =
  ## Records the platform's reading of the displays; an error keeps an earlier good reading.
  var reason = error
  if error.len == 0:
    if displays.len > 0:
      c.displays = displays
      c.monitorDetection = MonitorDetection(kind: mdFound)
      return none(string)
    reason = "the platform reported no displays"
  if c.displays.len == 0:
    c.monitorDetection = MonitorDetection(kind: mdFailed, reason: reason)
    none(string)
  else:
    c.monitorDetection = MonitorDetection(kind: mdRetained, reason: reason)
    some(reason)

# ---------------------------------------------------------------- conversion

proc intentOr(p: JsonNode; default: Intent): Intent =
  let s = p.pget("intent").asStr
  if s.isNone: return default
  let i = parseIntent(s.get)
  if i.isNone: raise other("unknown intent `" & s.get & "` (perceptual|relative|saturation|absolute)")
  i.get

proc profileJson(p: Profile): JsonNode =
  var intents = newJArray()
  for i in Intent:
    if p.supportsIntent(i, false): intents.add %i.id
  var lut = false
  for a in p.a2b:
    if a.isSome: lut = true
  %*{"description": p.description, "colorSpace": csDebug(p.colorSpace), "class": classDebug(p.class),
     "version": $p.version[0] & "." & $(p.version[1] shr 4), "bytes": p.toBytes.len, "lut": lut, "intents": intents}

proc convertSurface*(s: Surface; frm: ColorMode; to0: PixelFormat; t: Transform): Surface =
  ## Converts one surface's colour channels with `t` into `to0` (alpha copied). Surfaces whose
  ## model is not `frm` fall back to the generic per-pixel conversion.
  let sf = s.format
  if sf.mode != frm or t.inputs != sf.mode.colorChannels or t.outputs != to0.mode.colorChannels or sf.alpha != to0.alpha:
    return s.convert(to0)
  let ss = sf.channels
  let ds = to0.channels
  let to = PixelFormat(mode: to0.mode, sample: sf.sample, alpha: to0.alpha)
  let dp = s.defaultPixel
  var dpo = newSeq[float32](ds)
  t.convertF32(dp, ss, dpo, ds, true)
  result = surfaceWithDefault(to, dpo)
  for c in s.sortedCoords:
    let src = s.tile(c)
    let dst = result.tileMut(c)
    case sf.sample
    of stU8:
      var outp = newSeq[uint8](TilePixels * ds)
      t.convertU8(src.data, ss, outp, ds, true)
      for i in 0 ..< outp.len: dst.data[i] = outp[i]
    of stU16:
      var inp = newSeq[uint16](TilePixels * ss)
      copyMem(addr inp[0], unsafeAddr src.data[0], inp.len * 2)
      var outp = newSeq[uint16](TilePixels * ds)
      t.convertU16(inp, ss, outp, ds, true)
      copyMem(addr dst.data[0], addr outp[0], outp.len * 2)
    of stF32:
      var inp = newSeq[float32](TilePixels * ss)
      copyMem(addr inp[0], unsafeAddr src.data[0], inp.len * 4)
      var outp = newSeq[float32](TilePixels * ds)
      t.convertF32(inp, ss, outp, ds, true)
      copyMem(addr dst.data[0], addr outp[0], outp.len * 4)

proc convertColor(c: var Color; frm, to: ColorMode; t: Transform) =
  ## Converts a colour value that is in `frm` (the document's old mode) to the new mode.
  if c.mode != frm: return
  let n = frm.colorChannels
  var o: array[16, float32]
  t.eval(c.c.toOpenArray(0, n - 1), o)
  let m = to.colorChannels
  var nc: array[4, float32]
  for i in 0 ..< min(m, 4): nc[i] = clamp(o[i], 0'f32, 1'f32)
  c.mode = to
  c.c = nc

proc convertFill(f: var Fill; cc: proc(c: var Color)) =
  case f.kind
  of fkSolid: cc(f.color)
  of fkGradient:
    for s in f.stops.mitems: cc(s[1])
  of fkPattern: discard

proc paintColors(p: var FxPaint; cc: proc(c: var Color)) =
  case p.kind
  of fpColor: cc(p.color)
  of fpGradient:
    for s in p.gradient.stops.mitems: cc(s[1])
  of fpPattern: discard

proc convertLayerColors*(l: var Layer; cc: proc(c: var Color)) =
  ## Applies `cc` to every colour value of `l` (fill, text, shape and effect colours).
  for e in l.effects.items.mitems:
    case e.kind
    of ekDropShadow, ekInnerShadow: cc(e.shadow.color)
    of ekOuterGlow, ekInnerGlow: paintColors(e.glow.paint, cc)
    of ekStroke: paintColors(e.stroke.paint, cc)
    of ekColorOverlay: cc(e.color)
    of ekGradientOverlay:
      for s in e.gradient.stops.mitems: cc(s[1])
    of ekSatin: cc(e.satin.color)
    of ekBevelEmboss:
      cc(e.bevel.highlightColor)
      cc(e.bevel.shadowColor)
    of ekPatternOverlay: discard
  case l.content.kind
  of lkFill: convertFill(l.content.fill, cc)
  of lkText:
    cc(l.content.text.color)
    for r in l.content.text.runs.mitems: cc(r.style.color)
  of lkShape:
    if l.content.shape.fill.isSome: convertFill(l.content.shape.fill.get, cc)
    if l.content.shape.stroke.isSome: convertFill(l.content.shape.stroke.get.paint, cc)
  of lkGroup:
    for c in l.content.group.children.mitems: convertLayerColors(c, cc)
  else: discard

proc inkSurface(s: Surface; d: Duotone): Surface =
  ## The RGB surface of a gray surface's ink colours (same depth and alpha).
  let sf = s.format
  if sf.mode != cmGrayscale: return s
  let to = PixelFormat(mode: cmRgb, sample: sf.sample, alpha: sf.alpha)
  let ss = sf.channels
  let ds = to.channels
  proc ink(src: openArray[float32]; outp: var openArray[float32]) =
    let rgbv = d.render(if src.len > 0: src[0] else: 0'f32)
    var vals = @[rgbv[0], rgbv[1], rgbv[2]]
    if src.len > 1: vals.add src[1]
    for i in 0 ..< min(outp.len, vals.len): outp[i] = vals[i]
  var dp = newSeq[float32](ds)
  ink(s.defaultPixel, dp)
  result = surfaceWithDefault(to, dp)
  var px = newSeq[float32](ss)
  var o = newSeq[float32](ds)
  for c in s.sortedCoords:
    let src = s.tile(c)
    let n = src.data.len div max(sf.bytesPerPixel, 1)
    let dst = result.tileMut(c)
    for i in 0 ..< n:
      for k in 0 ..< ss: px[k] = readSample(src.data, sf.sample, i * ss + k)
      ink(px, o)
      for k in 0 ..< ds: writeSample(dst.data, to.sample, i * ds + k, o[k])

proc inkDuotone(doc: var Document) =
  ## Turns a Duotone document into the sRGB document it displays as.
  if doc.mode != cmDuotone or doc.duotone.isNone or doc.duotone.get.inks.len == 0: return
  let d = doc.duotone.get
  doc.duotone = none(Duotone)
  forEachSurface(doc.layers, false, proc(surf: var Surface; isMask: bool) = surf = inkSurface(surf, d))
  for l in doc.layers.mitems:
    convertLayerColors(l, proc(c: var Color) =
      if c.mode in {cmGrayscale, cmDuotone}:
        let v = d.render(c.c[0])
        c = rgba(v[0], v[1], v[2], c.alpha))
  doc.mode = cmRgb
  doc.iccProfile = shared(bSrgb.profile.toBytes)

proc convertDocument*(doc: var Document; dst: Profile; intent: Intent; bpc: bool) =
  ## Converts every pixel layer (and caches) of `doc` from its profile to `dst`, switching the
  ## mode to `dst`'s colour space; fill, text, shape and effect colours too. Tags with `dst`.
  let toModeO = spaceMode(dst.colorSpace)
  if toModeO.isNone: raise other("cannot convert to a " & csDebug(dst.colorSpace) & " profile")
  let toMode = toModeO.get
  if toMode != cmGrayscale: inkDuotone(doc)
  let src = documentProfile(doc)
  let fromMode = doc.pixelFormat.mode
  var t: Transform
  try: t = newTransform(src, dst, intent, bpc)
  except CatchableError as e: raise cmsErr(e)
  let fmt = PixelFormat(mode: toMode, sample: doc.depth, alpha: true)
  forEachSurface(doc.layers, false, proc(surf: var Surface; isMask: bool) =
    let f = PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: surf.format.alpha)
    surf = convertSurface(surf, fromMode, f, t))
  for l in doc.layers.mitems:
    convertLayerColors(l, proc(c: var Color) = convertColor(c, fromMode, toMode, t))
  doc.mode = toMode
  doc.iccProfile = shared(dst.toBytes)
  doc.colorTable = none(ColorTable)
  doc.duotone = none(Duotone)

proc convertFromMultichannel*(s: var Session; mode: ColorMode; p: JsonNode): JsonNode

proc convertMode*(s: var Session; mode: ColorMode; p: JsonNode): JsonNode =
  ## Image › Mode › RGB/Grayscale/CMYK/Lab through the CMS.
  let doc = s.activeOrRaise().doc
  if doc.mode == cmMultichannel and mode != cmMultichannel: return convertFromMultichannel(s, mode, p)
  if doc.mode == mode and p.pget("profile").isNil: return jnull()
  let spec = p.pget("profile").asStr
  let dst = if spec.isSome: s.color.resolve(spec.get, some(doc[]), some(mode)) else: s.color.working(mode)
  let want = case mode
    of cmBitmap, cmDuotone: cmGrayscale
    of cmIndexed, cmMultichannel: cmRgb
    else: mode
  if spaceMode(dst.colorSpace) != some(want):
    raise other("profile `" & dst.description & "` is " & csDebug(dst.colorSpace) & ", not " & $mode)
  let intent = intentOr(p, s.color.settings.settingsIntent)
  let bpc = p.getBoolOr("bpc", s.color.settings.bpc)
  s.editDo("Mode Change"): convertDocument(doc, dst, intent, bpc)
  %*{"profile": dst.description, "intent": intent.id, "bpc": bpc}

proc gamutMask*(doc: Document; setup: ProofSetup; threshold: float32): (seq[uint8], int) =
  ## Out-of-gamut mask of the composite against `setup`'s profile, plus the count.
  var check: GamutCheck
  try: check = newGamutCheck(compositeProfile(doc), setup.profile, threshold)
  except CatchableError as e: raise cmsErr(e)
  let buf = flatten(doc)
  var count = 0
  var mask = newSeq[uint8](buf.px.len)
  for i, p in buf.px:
    if p[3] > 0 and check.outOfGamut(p.toOpenArray(0, 2)):
      inc count
      mask[i] = 255
  (mask, count)

# ---------------------------------------------------------------- commands

proc assignProfile(s: var Session; p: JsonNode): JsonNode =
  let spec = p.pget("profile").asStr
  if spec.isNone: raise bad("edit.assignProfile", "missing `profile`")
  let doc = s.activeOrRaise().doc
  var bytes: SharedBytes
  if spec.get != "none":
    let prof = s.color.resolve(spec.get, some(doc[]), none(ColorMode))
    if prof.colorSpace != modeSpace(doc.mode):
      raise other("profile `" & prof.description & "` is " & csDebug(prof.colorSpace) & "; the document is " & $doc.mode)
    bytes = shared(prof.toBytes)
  var desc = "none"
  if not bytes.isNil:
    try: desc = profileFromBytes(bytes).description
    except CatchableError: desc = ""
  s.editDo("Assign Profile"): doc.iccProfile = bytes
  %*{"profile": desc}

proc convertToProfile(s: var Session; p: JsonNode): JsonNode =
  let spec = p.pget("profile").asStr
  if spec.isNone: raise bad("edit.convertToProfile", "missing `profile`")
  let doc = s.activeOrRaise().doc
  let dst = s.color.resolve(spec.get, some(doc[]), none(ColorMode))
  let intent = intentOr(p, s.color.settings.settingsIntent)
  let bpc = p.getBoolOr("bpc", s.color.settings.bpc)
  s.editDo("Convert to Profile"): convertDocument(doc, dst, intent, bpc)
  let d = s.activeOrRaise().doc
  %*{"profile": dst.description, "mode": $d.mode, "intent": intent.id, "bpc": bpc}

proc profileInfo(s: var Session; p: JsonNode): JsonNode =
  var builtins = newJArray()
  for b in Builtin: builtins.add %*{"id": b.id, "description": b.description, "colorSpace": csDebug(b.profile.colorSpace)}
  let d = s.active
  let doc = if d.isNil: none(Document) else: some(d.doc[])
  var info = newJNull()
  let spec = p.pget("profile").asStr
  if spec.isSome: info = profileJson(resolveProfile(spec.get, doc, none(ColorMode)))
  elif doc.isSome:
    info = profileJson(documentProfile(doc.get))
    info["embedded"] = %(not doc.get.iccProfile.isNil)
  %*{"profile": info, "builtins": builtins}

proc proofSetupCmd(s: var Session; p: JsonNode): JsonNode =
  let st = s.activeOrRaise()
  let id = st.doc.id
  let doc = st.doc[]
  let name = p.getStrOr("profile", "working-cmyk")
  let profile = s.color.resolve(name, some(doc), some(cmCmyk))
  let intent = intentOr(p, RelativeColorimetric)
  let setup = ProofSetup(name: name, profile: profile, intent: intent, bpc: p.getBoolOr("bpc", true),
                         simulatePaper: p.getBoolOr("simulatePaper", false), kind: ProofKind(kind: pkProfile))
  s.color.proofMut(id).setup = setup
  %*{"profile": setup.profile.description, "intent": setup.intent.id, "bpc": setup.bpc,
     "simulatePaper": setup.simulatePaper, "proofColors": s.color.proofMut(id).enabled}

proc proofColorsCmd(s: var Session; p: JsonNode): JsonNode =
  let id = s.activeOrRaise().doc.id
  template pv: untyped = s.color.proofMut(id)
  pv.enabled = p.getBoolOr("on", not pv.enabled)
  %*{"on": pv.enabled, "profile": pv.setup.profile.description}

proc gamutWarningCmd(s: var Session; p: JsonNode): JsonNode =
  let doc = s.activeOrRaise().doc[]
  template pv: untyped = s.color.proofMut(doc.id)
  pv.gamutWarning = p.getBoolOr("on", not pv.gamutWarning)
  let t = p.pget("threshold").asF64
  if t.isSome: pv.gamutThreshold = float32(rmax(t.get, 0.0))
  var setup = pv.setup
  let on = pv.gamutWarning
  let threshold = pv.gamutThreshold
  let spec = p.pget("profile").asStr
  if spec.isSome: setup.profile = resolveProfile(spec.get, some(doc), some(cmCmyk))
  let (_, count) = gamutMask(doc, setup, threshold)
  let total = doc.size.width * doc.size.height
  %*{"on": on, "profile": setup.profile.description, "threshold": threshold, "outOfGamut": count,
     "fraction": float64(count) / float64(max(total, 1))}

proc settingsJson*(c: ColorSettings): JsonNode =
  %*{"workingRgb": c.workingRgb, "workingCmyk": c.workingCmyk, "workingGray": c.workingGray,
     "policyRgb": $c.policyRgb, "policyCmyk": $c.policyCmyk, "policyGray": $c.policyGray,
     "askOnMismatch": c.askOnMismatch, "askOnPaste": c.askOnPaste, "askOnMissing": c.askOnMissing,
     "intent": c.intent, "bpc": c.bpc, "dither": c.dither, "blendTextGamma": c.blendTextGamma,
     "monitorProfile": c.monitorProfile}

proc prefsTouch*(s: var Session)

proc colorSettingsCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "edit.colorSettings"
  var next = s.color.settings
  if p.pget("reset").asBool == some(true): next = defaultColorSettings()
  proc strOf(k: string): Option[string] = p.pget(k).asStr
  if strOf("workingRgb").isSome: next.workingRgb = strOf("workingRgb").get
  if strOf("workingCmyk").isSome: next.workingCmyk = strOf("workingCmyk").get
  if strOf("workingGray").isSome: next.workingGray = strOf("workingGray").get
  if strOf("monitorProfile").isSome: next.monitorProfile = strOf("monitorProfile").get
  for k in ["policyRgb", "policyCmyk", "policyGray"]:
    let v = p.pget(k).asStr
    if v.isSome:
      let pol = policyParse(v.get)
      if pol.isNone: raise bad(cmdId, "`" & k & "` must be preserve|convert|off")
      case k
      of "policyRgb": next.policyRgb = pol.get
      of "policyCmyk": next.policyCmyk = pol.get
      else: next.policyGray = pol.get
  for k in ["askOnMismatch", "askOnPaste", "askOnMissing", "bpc", "dither"]:
    let v = p.pget(k).asBool
    if v.isSome:
      case k
      of "askOnMismatch": next.askOnMismatch = v.get
      of "askOnPaste": next.askOnPaste = v.get
      of "askOnMissing": next.askOnMissing = v.get
      of "bpc": next.bpc = v.get
      else: next.dither = v.get
  let iv = strOf("intent")
  if iv.isSome:
    let i = parseIntent(iv.get)
    next.intent = if i.isSome: i.get.id else: iv.get
  let g = p.pget("blendTextGamma")
  if not g.isNil:
    if g.kind == JBool and not g.bval: next.blendTextGamma = 1
    elif g.kind == JBool: next.blendTextGamma = TextGammaDefault
    else:
      let f = g.asF64
      if f.isNone or not (f.get >= 1.0 and f.get <= 2.2): raise bad(cmdId, "`blendTextGamma` must be 1.0..2.2 or a bool")
      next.blendTextGamma = float32(f.get)
  let why = validateSettings(next)
  if why.len > 0: raise bad(cmdId, why)
  if next != s.color.settings:
    setTextGamma(next.blendTextGamma)
    s.color.settings = next
    # Persisted with the preferences.
    prefsTouch(s)
  let c = s.color.settings
  %*{"settings": settingsJson(c),
     "working": {"rgb": s.color.working(cmRgb).description, "cmyk": s.color.working(cmCmyk).description,
                 "gray": s.color.working(cmGrayscale).description},
     "monitor": s.color.monitor.description, "monitorDetected": s.color.displays.len > 0,
     "monitorStatus": s.color.monitorStatus, "displays": s.color.displayStatuses}

proc profileMismatch(s: var Session; p: JsonNode): JsonNode =
  let action = p.getStrOr("action", "preserve")
  let mode = s.activeOrRaise().doc.mode
  case action
  of "preserve", "useEmbedded": %*{"action": "kept"}
  of "convert", "convertToWorking":
    let dst = s.color.working(mode)
    let intent = s.color.settings.settingsIntent
    let bpc = s.color.settings.bpc
    s.editDo("Convert to Working RGB"): convertDocument(doc, dst, intent, bpc)
    %*{"action": "converted", "profile": dst.description}
  of "discard", "off":
    s.editDo("Discard Profile"): doc.iccProfile = nil
    %*{"action": "discarded"}
  of "assignWorking":
    let w = s.color.working(mode)
    s.editDo("Assign Profile"): doc.iccProfile = shared(w.toBytes)
    %*{"action": "assigned", "profile": w.description}
  else: raise bad("color.profileMismatch", "unknown action `" & action & "` (preserve|convert|discard|assignWorking)")

proc ignoreExifProfileTag*(s: Session): bool

proc openDocument*(s: var Session; doc: Document; path = none(string)): (int, JsonNode) =
  ## Adds a document read from a file, applying the Color Settings policy first.
  var d = doc
  let report = s.color.openPolicy(d, ignoreExifProfileTag(s))
  (s.addDocument(d, path), report)

proc colorSpecs*(): seq[CommandSpec] =
  @[
    cmd("edit.assignProfile", "Assign Profile…", ["Edit"], "",
        """{"profile":"working|srgb|display-p3|adobe-rgb-compat|prophoto-compat|linear-srgb|rec2020|gray-gamma-2.2|sgray|lab-d50|coated-cmyk|none" (or a path to an .icc file)}""",
        hasDocE, assignProfile),
    cmd("edit.convertToProfile", "Convert to Profile…", ["Edit"], "",
        """{"profile":"srgb|display-p3|adobe-rgb-compat|prophoto-compat|linear-srgb|rec2020|gray-gamma-2.2|sgray|lab-d50|coated-cmyk|working" (or a path to an .icc file),"intent":"perceptual|relative|saturation|absolute"="relative","bpc":bool=true}""",
        hasDocE, convertToProfile),
    cmd("edit.colorSettings", "Color Settings…", ["Edit"], "Cmd+Shift+K",
        """{"workingRgb":"srgb|display-p3|adobe-rgb-compat|prophoto-compat|linear-srgb|rec2020","workingCmyk":"coated-cmyk","workingGray":"sgray|gray-gamma-2.2","policyRgb":"preserve|convert|off","policyCmyk":"preserve|convert|off","policyGray":"preserve|convert|off","askOnMismatch":bool=true,"askOnPaste":bool=true,"askOnMissing":bool=false,"intent":"relative|perceptual|saturation|absolute","blendTextGamma":1.0..2.2|bool=1.45,"bpc":bool=true,"dither":bool=true,"monitorProfile":"auto|srgb|display-p3|adobe-rgb-compat|prophoto-compat|rec2020","reset":bool=false} (working spaces and the monitor profile also accept .icc paths; monitor `auto` = the main display's profile when the platform provides it, else sRGB; the reply's `monitorStatus` says which profile is in use and why: source auto|manual|fallback, reason)""",
        always, colorSettingsCmd),
    cmd("color.profileMismatch", "Embedded Profile Mismatch", [], "", """{"action":"preserve|convert|discard|assignWorking"}""",
        hasDocE, profileMismatch),
    cmd("edit.profileInfo", "Profile Info", [], "", """{"profile":"<builtin id>|document|/path/to/profile.icc"=document}""",
        always, profileInfo, journal = false),
    cmd("view.proofSetup", "Proof Setup…", [], "",
        """{"profile":"working-cmyk|srgb|display-p3|adobe-rgb-compat|prophoto-compat|linear-srgb|rec2020|gray-gamma-2.2|sgray|lab-d50|coated-cmyk" (or a path to an .icc file)="working-cmyk","intent":"perceptual|relative|saturation|absolute"="relative","bpc":bool=true,"simulatePaper":bool=false}""",
        hasDocE, proofSetupCmd, journal = false),
    cmd("view.proofColors", "Proof Colors", ["View"], "Cmd+Y", """{"on":bool=toggle}""", hasDocE, proofColorsCmd, journal = false),
    cmd("view.gamutWarning", "Gamut Warning", ["View"], "Cmd+Shift+Y",
        """{"on":bool=toggle,"threshold":deltaE=4,"profile":"<proof profile override>"}""", hasDocE, gamutWarningCmd,
        journal = false),
  ]
