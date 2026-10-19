## The Affinity archive (port of `affinity/src/container.rs`): header, the chain of file
## allocation tables (one per save) and bounded, CRC-checked extraction of named entries such as
## `doc.dat` and raster tiles. Also the crate's error type and the byte cursor.

import std/[tables, sets, strutils]
import ../zlib, ../zstd, ../textutil

type
  AffinityErrorKind* = enum
    afMalformed, afUnsupported, afLimit

  AffinityError* = object of CatchableError
    ## Why a file could not be read. Messages are short reasons; callers add advice.
    kind*: AffinityErrorKind
    detail*: string

proc afError*(kind: AffinityErrorKind; detail: string): ref AffinityError =
  ## The error with the Rust `Display` text as its message.
  let msg = case kind
    of afMalformed: "damaged Affinity file: " & detail
    of afUnsupported: "unsupported Affinity file: " & detail
    of afLimit: "Affinity file exceeds a safety limit: " & detail
  (ref AffinityError)(kind: kind, detail: detail, msg: msg)

proc malformed*(detail: string): ref AffinityError = afError(afMalformed, detail)
proc unsupported*(detail: string): ref AffinityError = afError(afUnsupported, detail)
proc limit*(detail: string): ref AffinityError = afError(afLimit, detail)

const
  Magic* = "\x00\xffKA"
  MinVersion = 8'u16 ## oldest and newest container versions seen in real documents
  MaxVersion = 12'u16
  DocumentKind = "nsrP" ## `Prsn` stored byte-reversed: documents (libraries share the magic)
  MaxTables = 4096
  MaxZstdWindow = 64 shl 20

type
  Limits* = object
    ## Decompression budget for one import, checked against declared sizes before allocating.
    maxEntry*: int ## largest single entry (`doc.dat` or one tile)
    maxTotal*: int ## total bytes extracted over the whole import

  Header* = object
    version*, flags*: uint16
    fatOffset*, thumbnailOffset*: uint64
    saved*: uint32 ## save time in Unix seconds

  Entry* = object
    ## One FAT record, as of the newest table that mentions its id.
    id*: uint32
    name*: string
    offset, size, compressed: uint64
    crc: uint32
    compression: uint8

  Archive* = object
    data: seq[byte]
    header*: Header
    entries: Table[string, Entry]
    revisions*: int ## number of tables (saves) in the chain
    extracted: int
    limits: Limits

  Cursor* = object
    data*: ptr UncheckedArray[byte]
    len*, pos*: int

proc defaultLimits*(): Limits = Limits(maxEntry: 256 shl 20, maxTotal: 1 shl 30)

proc size*(e: Entry): uint64 = e.size

proc cursor*(data: openArray[byte]; pos: int): Cursor =
  ## A cursor over `data`, which must outlive it.
  Cursor(data: (if data.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr data[0]) else: nil), len: data.len, pos: pos)

proc remaining*(c: Cursor): int = max(c.len - c.pos, 0)

proc takeRange*(c: var Cursor; n: int): (int, int) =
  ## Advances over `n` bytes and returns their range `(start, n)`.
  # Positions and lengths here are below 2^63, so their sum never overflows Rust's u64: an end
  # past `int` is past the end of the data.
  if n < 0 or c.pos > high(int) - n: raise malformed("truncated data")
  let e = c.pos + n
  if c.pos < 0 or e > c.len: raise malformed("truncated data")
  result = (c.pos, n)
  c.pos = e

proc take*(c: var Cursor; n: int): seq[byte] =
  let (s, k) = c.takeRange(n)
  result = newSeq[byte](k)
  for i in 0 ..< k: result[i] = c.data[s + i]

proc takeIs*(c: var Cursor; s: string): bool =
  ## Takes `s.len` bytes and compares them with `s`.
  let (at, k) = c.takeRange(s.len)
  for i in 0 ..< k:
    if c.data[at + i] != byte(s[i]): return false
  true

proc u8*(c: var Cursor): uint8 =
  let (at, _) = c.takeRange(1)
  c.data[at]

proc u16*(c: var Cursor): uint16 =
  let (at, _) = c.takeRange(2)
  uint16(c.data[at]) or (uint16(c.data[at + 1]) shl 8)

proc u32*(c: var Cursor): uint32 =
  let (at, _) = c.takeRange(4)
  for k in countdown(3, 0): result = (result shl 8) or uint32(c.data[at + k])

proc u64*(c: var Cursor): uint64 =
  let (at, _) = c.takeRange(8)
  for k in countdown(7, 0): result = (result shl 8) or uint64(c.data[at + k])

proc cursorAt(bytes: openArray[byte]; v: uint64): Cursor =
  ## A cursor at file offset `v`, whose first read is 4 bytes. Rust addresses any u64 offset: one
  ## beyond `int` fails that read with "length overflow" when `v + 4` wraps, else "truncated data".
  if v > uint64(high(int)):
    if v > high(uint64) - 4: raise malformed("length overflow")
    raise malformed("truncated data")
  cursor(bytes, int(v))

proc lengthOf(c: Cursor; v: uint64): int =
  ## A length to take at `c`; one beyond `int` fails as Rust's `take` does.
  if v > uint64(high(int)):
    if v > high(uint64) - uint64(c.pos): raise malformed("length overflow")
    raise malformed("truncated data")
  int(v)

proc isAffinity*(bytes: openArray[byte]): bool =
  ## True when `bytes` start with the Affinity container signature (documents and libraries).
  if bytes.len < 4: return false
  for i in 0 .. 3:
    if bytes[i] != byte(Magic[i]): return false
  true

proc header*(bytes: openArray[byte]): Header =
  ## Checks the signature, version and document kind.
  if not isAffinity(bytes): raise malformed("missing container signature")
  var c = cursor(bytes, 4)
  let version = c.u16
  let flags = c.u16
  if version < MinVersion or version > MaxVersion: raise unsupported("unknown container version")
  if (flags and 3) != 0: raise unsupported("container variant")
  if not c.takeIs(DocumentKind):
    raise unsupported("this Affinity file is a library (assets, brushes or styles), not a document")
  if not c.takeIs("#Inf"): raise malformed("missing information header")
  let fatOffset = c.u64
  let thumbnailOffset = c.u64
  discard c.takeRange(16)
  let s = c.u64
  let saved = if s <= uint64(high(uint32)): uint32(s) else: 0'u32
  discard c.takeRange(8)
  if not c.takeIs("Prot"): raise malformed("missing protocol header")
  Header(version: version, flags: flags, fatOffset: fatOffset, thumbnailOffset: thumbnailOffset, saved: saved)

proc openArchive*(bytes: openArray[byte]; limits: Limits): Archive =
  let h = header(bytes)
  # Newest record per id: (table date, table index, record).
  var latest: Table[uint32, (uint64, int, bool, Entry)]
  var names: Table[uint32, string]
  var seen: HashSet[uint64]
  var next = h.fatOffset
  var tables = 0
  while next != 0:
    if next in seen: raise malformed("allocation tables form a cycle")
    seen.incl next
    inc tables
    if tables > MaxTables: raise limit("too many saved revisions")
    var c = cursorAt(bytes, next)
    let (tagAt, _) = c.takeRange(4)
    var tag = ""
    for i in 0 ..< 4: tag.add char(c.data[tagAt + i])
    let level = case tag
      of "#FAT": 1
      of "#FT2": 2
      of "#FT3": 3
      of "#FT4": 4
      else: raise malformed("missing allocation table")
    next = c.u64
    let date = c.u64
    discard c.takeRange(24)
    let files = c.u32
    discard c.takeRange(8)
    let dirs = c.u16
    discard c.takeRange(1)
    for _ in 0'u32 ..< files:
      let id = c.u32
      let flag = c.u8
      var record: Entry
      var has = false
      case flag
      of 0, 1:
        let offset = c.u64
        let size = c.u64
        let compressed = c.u64
        let crc = c.u32
        let stored = c.u8
        if level >= 2: discard c.takeRange(4)
        if level >= 4: discard c.takeRange(4)
        # Old tables number the methods; newer ones store the method byte itself.
        let compression =
          if level <= 2:
            case stored
            of 1: 0x01'u8
            of 2: 0x41'u8
            of 3: 0x81'u8
            of 4: 0xC1'u8
            else: 0'u8
          else: stored
        record = Entry(id: id, offset: offset, size: size, compressed: compressed, crc: crc, compression: compression)
        has = true
      of 2: discard
      else: raise malformed("unknown allocation record")
      if flag == 0:
        let n = int(c.u16)
        let name = lossyUtf8(c.take(n))
        if id notin names: names[id] = name
      # Equal dates: the first table in the chain (the newest) wins, as in afread.
      if id notin latest or date > latest[id][0]: latest[id] = (date, tables, has, record)
    for _ in 0'u16 ..< dirs:
      let n = int(c.u16)
      discard c.takeRange(10)
      discard c.takeRange(n)
  result = Archive(data: @bytes, header: h, revisions: tables, limits: limits)
  for id, v in latest:
    if v[2] and id in names:
      var e = v[3]
      e.name = names[id]
      result.entries[e.name] = e

proc entry*(a: Archive; name: string): (bool, Entry) =
  if name in a.entries: (true, a.entries[name]) else: (false, Entry())

iterator entryNames*(a: Archive): string =
  for k in a.entries.keys: yield k

proc extracted*(a: Archive): int = a.extracted
  ## Bytes extracted so far, counted against `Limits.maxTotal`.

proc unzstd(data: openArray[byte]; size: int): seq[byte] =
  ## Entries may hold several concatenated frames; each is read as `ruzstd::StreamingDecoder`
  ## through a bounded reader is, with a 64 MiB window ceiling.
  var pos = 0
  while pos < data.len:
    let left = size - result.len
    var frame: seq[byte]
    try: frame = zstdReadFrame(data, pos, if left < high(int): left + 1 else: left, MaxZstdWindow)
    except ZstdError: raise malformed("corrupt compressed entry")
    if frame.len == 0: raise malformed("corrupt compressed entry")
    result.add frame
    if result.len > size: raise malformed("archive entry size mismatch")

proc byteDelta(b: var seq[byte]) =
  var prev = 0'u8
  for v in b.mitems:
    v = v + prev
    prev = v

proc unpredict(b: var seq[byte]; compression: uint8) =
  ## Undoes the byte predictor selected by the high bits of the compression byte.
  let hi = compression and 0xC0
  let flag = (compression and 0x20) != 0
  if hi == 0x00: discard
  elif hi == 0x40: byteDelta(b)
  elif hi == 0x80 and not flag:
    var prev = 0'u16
    var i = 0
    while i + 1 < b.len:
      let v = (uint16(b[i]) or (uint16(b[i + 1]) shl 8)) + prev
      b[i] = byte(v and 0xFF)
      b[i + 1] = byte(v shr 8)
      prev = v
      i += 2
  elif hi == 0x80:
    byteDelta(b)
    # A 64 KiB tile of 16-bit samples is stored as all high bytes, then all low bytes.
    if b.len == 0x10000:
      let planes = b
      for i in 0 ..< 0x10000 div 4:
        let (h0, h1, l0, l1) = (planes[2 * i], planes[2 * i + 1], planes[0x8000 + 2 * i], planes[0x8000 + 2 * i + 1])
        b[4 * i] = l0
        b[4 * i + 1] = h0
        b[4 * i + 2] = l1
        b[4 * i + 3] = h1
  else: raise unsupported("archive entry predictor")

proc read*(a: var Archive; name: string): seq[byte] =
  ## Extracts an entry by name: decompress, undo the byte predictor and verify the CRC.
  if name notin a.entries: raise malformed("missing archive entry")
  let e = a.entries[name]
  if e.size > uint64(high(int)): raise limit("archive entry too large")
  let size = int(e.size)
  if size > a.limits.maxEntry: raise limit("archive entry too large")
  if a.extracted > high(int) - size: raise limit("document too large")
  let total = a.extracted + size
  if total > a.limits.maxTotal: raise limit("document too large")
  var c = cursorAt(a.data, e.offset)
  if not c.takeIs("#Fil"): raise malformed("archive entry has no data block")
  let meth = e.compression and 3
  let storedLen = if meth in [1'u8, 2'u8]: e.compressed else: e.size
  let (at, n) = c.takeRange(c.lengthOf(storedLen))
  var outp: seq[byte]
  case meth
  of 1:
    try: outp = zlibReadBounded(a.data.toOpenArray(at, at + n - 1), if size < high(int): size + 1 else: size)
    except ZlibError: raise malformed("corrupt compressed entry")
  of 2:
    # Entries may hold several concatenated frames; the decoder reads them all.
    if n > 0: outp = unzstd(a.data.toOpenArray(at, at + n - 1), size)
  else: outp = a.data[at ..< at + n]
  if outp.len != size: raise malformed("archive entry size mismatch")
  unpredict(outp, e.compression)
  if crc32(outp) != e.crc: raise malformed("archive entry checksum mismatch")
  a.extracted = total
  outp
