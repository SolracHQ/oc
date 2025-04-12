import os

type FileInfo* = ref object
  path*: string
  content*: string

proc newFileInfo*(path: string): FileInfo =
  let content = readFile(path)
  result = FileInfo(path: path, content: content)

proc name*(f: FileInfo): string =
  result = f.path.splitFile().name