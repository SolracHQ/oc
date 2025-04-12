import types
import position
import ast
import std/tables
import std/options

type
  ScopeKind* = enum
    ModuleScope # Represents the top-level scope of a module
    FunctionScope # Represents the scope of a function
    BlockScope # Represents the scope of a block (e.g., if, while)

  SymbolKind* = enum
    Variable # Represents a variable
    Function # Represents a function
    Type

  Symbol* = ref object
    name*: string
    canonicalName*: string # The C name of the symbol
    pos*: Position
    isPublic*: bool
    case kind*: SymbolKind
    of Variable:
      varType*: Type
      isReadOnly*: bool
      isGlobal*: bool
      isInitialized*: bool
    of Function:
      returnType*: Type
      paramTypes*: seq[Type]
    of Type:
      typeRepr*: Type

  Scope* = ref object
    kind*: ScopeKind
    parent*: Scope
    symbols*: Table[string, Symbol]
    children*: Table[string, Scope]
    name*: string

proc newScope*(kind: ScopeKind, parent: Scope, name: string): Scope =
  result = Scope( kind: kind, parent: parent, name: name)
  if parent != nil:
    if parent.children.hasKey(name):
      raise newException(ValueError, "Scope already exists: " & name)
    parent.children[name] = result

proc calculateCanonicalName*(scope: Scope, name: string): string =
  ## Canonical name is __{symbol_name}_at_{scope_name}_at_{parent_scope_name}_at_..
  var canonicalName = "__" & name
  var currentScope = scope
  while currentScope != nil:
    if currentScope.name.len > 0:
      canonicalName.add("_at_" & currentScope.name)
    currentScope = currentScope.parent
  result = canonicalName

proc addSymbol*(scope: Scope, name: string, symbol: Symbol): bool =
  if not scope.symbols.hasKey(name):
    symbol.canonicalName = calculateCanonicalName(scope, name)
    scope.symbols[name] = symbol
    return true
  result = false

proc findSymbol*(scope: Scope, name: string, at: Position): Option[Symbol] =
  var currentScope = scope
  var skipToModule = false

  while currentScope != nil:
    # Check if this scope has the symbol
    if currentScope.symbols.hasKey(name):
      let symbol = currentScope.symbols[name]
      # check if at moment is after the symbol definition or symbol is global (functions and types are global)
      if symbol.kind == Variable and symbol.isGlobal:
        return some(symbol)
      elif symbol.kind == Function:
        return some(symbol)
      elif symbol.kind == Type:
        return some(symbol)
      elif at >= symbol.pos:
        return some(symbol)
    
    # Determine if we need to skip scopes
    if not skipToModule and currentScope.kind == FunctionScope:
      skipToModule = true
    
    # Get next scope to check based on our rules
    currentScope = currentScope.parent
    
    # If skipping to module, skip all non-module scopes in one go
    while skipToModule and currentScope != nil and currentScope.kind != ModuleScope:
      currentScope = currentScope.parent
  
  # Symbol not found
  return none(Symbol)