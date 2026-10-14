## Fonts a host serves next to the app, fetched on demand (the Rust web build). The native port
## has no served fonts: requests are recorded for a host that wants them, and no script fallback
## families are added.

import std/[locks]

var pending: seq[string]
var pendingLock: Lock
initLock(pendingLock)

proc request*(family: string) =
  ## Asks the host for `family` (recorded; nothing fetches it natively).
  withLock pendingLock:
    if family.len > 0 and family notin pending and pending.len < 64: pending.add family

proc requestForText*(text: string) = discard
  ## Scripts in `text` that need a served font (none natively).

proc takeRequests*(): seq[string] =
  withLock pendingLock:
    result = pending
    pending.setLen(0)

proc scriptFallbackFamilies*(): seq[string] = @[]
  ## Served families that are a script's fallback (none natively).
