import ../types/file_info
import ../code_generator/generator
import std/osproc
import std/os

proc compile*(
    file: file_info.FileInfo, compiler: string = "gcc", cflags: string = ""
): bool =
  ## Compiles the generated C file using the specified C compiler and flags
  if not generateCode(file):
    echo "Error: Failed to generate code for ", file.name
    return false
  let cacheDir = getCurrentDir() / ".oc-cache"
  let cFile = cacheDir / (file.name & ".c")
  let outFile = cacheDir / file.name
  if not fileExists(cFile):
    echo "Error: C file not found: ", cFile
    return false
  let cmd = compiler & " " & cflags & " -o '" & outFile & "' '" & cFile & "'"
  echo "[oc] $ ", cmd
  let (output, exitCode) = execCmdEx(cmd)
  if exitCode != 0:
    echo output
    echo "Error: Compilation failed."
    return false
  echo "Compiled successfully: ", outFile
  return true
