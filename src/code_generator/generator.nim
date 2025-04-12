import ../types/[scope]

import std/sets

type
  # Represents a code generator
  CodeGenerator* = object
    globalSymbols*: seq[Symbol]