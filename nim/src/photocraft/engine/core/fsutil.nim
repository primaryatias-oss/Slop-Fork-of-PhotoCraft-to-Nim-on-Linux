## File access with Rust's `std::io::Error` messages ("No such file or directory (os error 2)"),
## so errors read the same as the Rust build's. Part of the `engine/core` unit: included, not
## imported.

proc osErrorText*(code: OSErrorCode): string =
  ## Rust's `Display` for an OS error.
  $strerror(cint(code)) & " (os error " & $int(code) & ")"

proc readBytesRust*(path: string): seq[byte] =
  ## `std::fs::read(path)`, raising `OSError` with Rust's message (without the path).
  when defined(js) or defined(wasm32):
    raise newException(OSError, "no file system")
  else:
    let fd = posix.open(path.cstring, O_RDONLY or O_CLOEXEC)
    if fd < 0: raise newException(OSError, osErrorText(osLastError()))
    defer: discard posix.close(fd)
    var st: Stat
    if fstat(fd, st) != 0: raise newException(OSError, osErrorText(osLastError()))
    if S_ISDIR(st.st_mode): raise newException(OSError, osErrorText(OSErrorCode(EISDIR)))
    var buf: array[65536, byte]
    while true:
      let n = posix.read(fd, addr buf[0], buf.len)
      if n < 0:
        let e = osLastError()
        if int(e) == EINTR: continue
        raise newException(OSError, osErrorText(e))
      if n == 0: break
      let at = result.len
      result.setLen(at + n)
      copyMem(addr result[at], addr buf[0], n)

proc readFileOr*(path: string): seq[byte] =
  ## `std::fs::read(path).map_err(|e| Other(format!("{path}: {e}")))`.
  try: readBytesRust(path)
  except OSError as e: raise other(path & ": " & e.msg)

proc writeFileAtomic*(path: string; bytes: openArray[byte]) =
  ## `file_cmds::write_file`: an atomic write, errors as `Other`.
  try: atomicWrite(path, bytes)
  except CatchableError as e: raise other(e.msg)
