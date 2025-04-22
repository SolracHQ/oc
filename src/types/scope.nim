import position
import ast
import types
import std/tables
import std/options

type
  ScopeKind* = enum
    ModuleScope # Represents the top-level scope of a module
    FunctionScope # Represents the scope of a function
    BlockScope # Represents the scope of a block (e.g., if, while)

  SymbolKind* = enum
    Type # Represents a type
    Function # Represents a function
    Variable # Represents a variable

  Symbol* = ref object
    name*: string
    canonicalName*: string # The C name of the symbol
    declStmt*: Stmt # The declaration statement of the symbol
    scope*: Scope # The scope in which the symbol is declared
    case kind*: SymbolKind
    of Variable:
      isInitialized*: bool
    of Function: discard
    of Type:
      typeRepr*: Type # The type representation of the symbol

  Scope* = ref object
    kind*: ScopeKind
    parent*: Scope
    symbols*: Table[string, Symbol]
    children*: Table[string, Scope]
    name*: string

const AnySymbol*: set[SymbolKind] = {Type, Function, Variable}

proc newScope*(kind: ScopeKind, parent: Scope, name: string): Scope =
  result = Scope(kind: kind, parent: parent, name: name)
  if parent != nil:
    if parent.children.hasKey(name):
      raise newException(ValueError, "Scope already exists: " & name)
    parent.children[name] = result

proc calculateCanonicalName*(scope: Scope, symbol: Symbol): string =
  ## Canonical name is __{symbol_name}_at_{scope_name}_at_{parent_scope_name}_at_..
  ## Only for module level names var, let declared globally or any function
  ## This is used to avoid name clashes in the generated C code
  if scope.kind != ModuleScope and symbol.kind == Variable:
    return symbol.name
  var canonicalName = "__" & symbol.name
  var currentScope = scope
  while currentScope != nil:
    if currentScope.name.len > 0:
      canonicalName.add("_at_" & currentScope.name)
    currentScope = currentScope.parent
  result = canonicalName

proc addSymbol*(scope: Scope, name: string, symbol: Symbol): bool =
  # Prevent any name conflict, regardless of SymbolKind
  if scope.symbols.hasKey(name):
    return false
  symbol.name = name
  symbol.canonicalName = calculateCanonicalName(scope, symbol)
  symbol.scope = scope
  scope.symbols[name] = symbol
  return true

proc findSymbol*(
    scope: Scope, name: string, at: Position, expected: set[SymbolKind]
): Option[Symbol] =
  var currentScope = scope
  var requireModuleLevelVariable = false
  while currentScope != nil:
    if currentScope.symbols.hasKey(name):
      let symbol = currentScope.symbols[name]
      if symbol.kind in expected:
        if symbol.kind == Variable:
          if requireModuleLevelVariable:
            if currentScope.kind == ModuleScope:
              return some(symbol)
          else:
            if currentScope.kind == ModuleScope or at >= symbol.declStmt.pos:
              return some(symbol)
        else:
          return some(symbol)
    if currentScope.kind == FunctionScope:
      requireModuleLevelVariable = true
    currentScope = currentScope.parent
  return none(Symbol)

proc findSymbol*(
    scope: Scope, name: string, at: Position, expected: SymbolKind
): Option[Symbol] {.inline.} =
  result = findSymbol(scope, name, at, {expected})

proc isReadOnly*(symbol: Symbol): bool =
  ## Check if a symbol is read-only
  ## Only variables can be read-only
  result =
    symbol.kind == Variable and symbol.declStmt.kind == SkVarDecl and
    symbol.declStmt.varDeclStmt.isReadOnly

proc isPublic*(symbol: Symbol): bool =
  ## Check if a symbol is public
  case symbol.kind
  of Variable:
    return symbol.declStmt.varDeclStmt.isPublic
  of Type:
    case symbol.declStmt.kind
    of SkTypeDecl:
      return symbol.declStmt.typeDeclStmt.isPublic
    of SkStructDecl:
      return symbol.declStmt.structDeclStmt.isPublic
    else:
      return false
  of Function:
    return symbol.declStmt.funDeclStmt.isPublic
