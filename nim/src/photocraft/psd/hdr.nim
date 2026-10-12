## HDR toning settings: the Color Mode Data of 32-bit documents. Photoshop refuses to open a
## 32-bit file whose Color Mode Data is not an `hdrt` record (layout observed from Photoshop files).

import ./io

const
  ToningKey* = "hdrt"
  ExtraKey* = "hdra"

proc isToningData*(data: openArray[byte]): bool =
  data.len >= 4 and char(data[0]) == 'h' and char(data[1]) == 'd' and char(data[2]) == 'r' and char(data[3]) == 't'

proc defaultToningData*(): seq[byte] =
  ## The 110-byte Color Mode Data Photoshop writes for a new 32-bit document.
  var o: seq[byte]
  o.put(ToningKey)
  o.putU32(3)
  o.putF32(0.23'f32)
  o.putU32(2)
  let name = utf16Units("Linear") & @[0'u16]
  o.putU32(uint32(name.len))
  for u in name: o.putU16(u)
  o.putU16(2)
  o.putU16(2)
  for (input, output) in [(0'u16, 0'u16), (255'u16, 255'u16)]:
    o.putU16(output)
    o.putU16(input)
  o.add [1'u8, 1]
  for _ in 0 ..< 8: o.add 0
  o.putF32(16.0'f32)
  o.putU32(1)
  o.putU32(0)
  o.putF32(1.0'f32)
  o.put(ExtraKey)
  o.putU32(6)
  o.putU32(0)
  o.putF32(20.0'f32)
  o.putF32(30.0'f32)
  o.putU32(0)
  o.putU32(0)
  o.putF32(1.0'f32)
  for _ in 0 ..< 6: o.add 0
  o

proc colorModeDataForDepth*(depth: int): seq[byte] =
  if depth == 32: defaultToningData() else: @[]
