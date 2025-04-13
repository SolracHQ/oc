import types/file_info
import code_generator/generator
import std/os

proc main() =
  let file = newFileInfo(paramStr(1))
  let codeGen = newCodeGenerator(file)
  if not codeGen.generateCode():
    echo "Code generation failed."
    return

when isMainModule:
  main()