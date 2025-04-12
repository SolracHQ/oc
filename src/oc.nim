# This is just an example to get you started. A typical binary package
# uses this file as the main entry point of the application.

import types/file_info
import parser/tree
import semantic_analysis/analyzer
import std/os

proc main() =
  let file = newFileInfo(paramStr(1))
  let analyzer = newAnalyzer(file)
  let (hasError, table, ast) = analyzer.analyze()
  echo treeRepr(ast)

when isMainModule:
  main()