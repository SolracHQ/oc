import ../types/file_info
import ../transpiler/transpiler
import std/os
import ./c_printer

proc generateCode*(file: file_info.FileInfo): bool =
  ## Generates code for the given scope
  # Return true if no errors
  result = true
  # Create cache dir for generated files, it should be on .oc-cache
  # Also copy core.h from core directory to cache dir
  let cacheDir = getCurrentDir() / ".oc-cache"
  if not dirExists(cacheDir):
    createDir(cacheDir)
  # Use the transpiler to get C ASTs
  let (hasError, hFile, cFile) = transpile(file)
  if hasError or hFile.isNil or cFile.isNil:
    return false
  # Generate header file
  let headerFileName = cacheDir / (file.name & ".h")
  let headerFile = open(headerFileName, fmWrite)
  headerFile.write(cStmtToString(hFile))
  headerFile.close()
  # Generate C file
  let cFileName = cacheDir / (file.name & ".c")
  let cFileHandle = open(cFileName, fmWrite)
  cFileHandle.write(cStmtToString(cFile))
  cFileHandle.close()
