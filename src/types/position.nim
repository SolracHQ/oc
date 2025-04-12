import file_info

type 
  Position* = object
    line*: int
    column*: int
    offset*: int # offset in bytes
    file*: FileInfo

proc `==`*(a, b: Position): bool =
  ## Compare two positions for equality
  result = a.line == b.line and a.column == b.column and a.offset == b.offset and a.file == b.file

proc `<`*(a, b: Position): bool =
  ## Compare two positions for less than
  result = a.line < b.line or (a.line == b.line and a.column < b.column) or (a.line == b.line and a.column == b.column and a.offset < b.offset)

proc `<=`*(a, b: Position): bool =
  ## Compare two positions for less than or equal
  result = a < b or a == b

proc `>`*(a, b: Position): bool =
  ## Compare two positions for greater than
  result = not (a <= b)

proc `>=`*(a, b: Position): bool =
  ## Compare two positions for greater than or equal
  result = not (a < b)