import types/file_info
import compiler/compiler
import std/os
import std/osproc
import std/parseopt

type
  BuildType = enum
    btDebug
    btRelease

  CliOptions = object
    subcommand: string
    filePath: string
    compiler: string
    cflags: string
    buildType: BuildType
    staticLink: bool

proc printHelp() =
  echo "OverC - transpile and compile OverC source files"
  echo "Usage: oc <subcommand> <file.oc> [options]"
  echo "Subcommands:"
  echo "  compile     Transpile and compile OverC source file (default)"
  echo "  run         Compile and run OverC source file"
  echo "Options:"
  echo "  --compiler=<c-compiler>   Set C compiler (default: gcc)"
  echo "  --cflags='<flags>'        Set C compiler flags (default: '')"
  echo "  --debug                  Build with debug flags (-O0)"
  echo "  --release                Build with release flags (-O3)"
  echo "  --static                 Statically link executable"
  echo "  -h, --help               Show this help message"

proc parseCliOptions(): CliOptions =
  result.compiler = ""
  result.cflags = ""
  result.buildType = btDebug
  result.staticLink = false
  result.subcommand = "compile"
  var p = initOptParser(commandLineParams())
  var positional: seq[string]
  while true:
    p.next()
    case p.kind
    of cmdEnd:
      break
    of cmdShortOption, cmdLongOption:
      case p.key
      of "compiler":
        result.compiler = p.val
      of "cflags":
        result.cflags = p.val
      of "debug":
        result.buildType = btDebug
      of "release":
        result.buildType = btRelease
      of "static":
        result.staticLink = true
      of "help", "h":
        printHelp()
        quit(0)
      else:
        echo "Unknown option: --", p.key
        printHelp()
        quit(1)
    of cmdArgument:
      positional.add(p.key)
  if positional.len == 0:
    printHelp()
    quit(1)
  if positional[0] in ["compile", "run"]:
    result.subcommand = positional[0]
    if positional.len < 2:
      printHelp()
      quit(1)
    result.filePath = positional[1]
  else:
    echo "Unknown subcommand: ", positional[0]
    printHelp()
    quit(1)

proc main() =
  let opts = parseCliOptions()
  let file = newFileInfo(opts.filePath)
  var cflags = opts.cflags
  if opts.buildType == btDebug:
    cflags &= " -O0"
  elif opts.buildType == btRelease:
    cflags &= " -O3"
  if opts.staticLink:
    cflags &= " -static"
  case opts.subcommand
  of "compile":
    if not compile(file, opts.compiler, cflags):
      quit(1)
  of "run":
    if not compile(file, opts.compiler, cflags):
      quit(1)
    let cacheDir = getCurrentDir() / ".oc-cache"
    let outFile = cacheDir / file.name
    echo "[oc] $ ", outFile
    let (output, exitCode) = execCmdEx(outFile)
    if exitCode != 0:
      stdout.write output
      quit(1)
    stdout.write output
  else:
    printHelp()
    quit(1)

when isMainModule:
  main()
