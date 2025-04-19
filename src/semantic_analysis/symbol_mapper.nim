import ../types/[file_info, position, scope, ast, types]
import ../reporter
import std/options

type SymbolMapper* = ref object
  fileInfo*: FileInfo
  scope*: Scope
  hasError*: bool

proc symbolMapperError*(symbolMapper: SymbolMapper, position: Position, msg: string) =
  ## Logs an error during symbol mapping
  logError("SymbolMapper", msg, position)
  symbolMapper.hasError = true

proc newSymbolMapper*(fileInfo: FileInfo, scope: Scope): SymbolMapper =
  ## Creates a new symbol mapper for the given file
  result = SymbolMapper()
  result.fileInfo = fileInfo
  result.scope = scope
  result.hasError = false

proc initializeTable*(symbolMapper: SymbolMapper, scope: Scope, node: Stmt) =
  ## Initializes the symbol table for the analyzer
  case node.kind
  of SkModule:
    # Process each statement in the module
    for stmt in node.moduleStmt.statements:
      initializeTable(symbolMapper, scope, stmt)
  of SkVarDecl:
    # Handle variable declarations
    var varDecl = node.varDeclStmt
    varDecl.typeAnnotation.hasAddress = true
    let varSymbol = Symbol(
      name: varDecl.identifier,
      node: node,
      kind: Variable,
      varType: varDecl.typeAnnotation,
      isGlobal: scope.kind == ModuleScope,
      isInitialized: varDecl.initializer.isSome,
    )

    # Check if symbol already exists in current scope
    if not scope.addSymbol(varDecl.identifier, varSymbol):
      symbolMapper.symbolMapperError(
        node.pos, "Symbol '" & varDecl.identifier & "' already defined in this scope"
      )

    # Process initializer if present
    if varDecl.initializer.isSome:
      # Initializer is an Expr, not a Stmt
      discard
  of SkFunDecl:
    # Handle function declarations
    let funDecl = node.funDeclStmt

    # do not allow public functions in inner scopes
    if funDecl.isPublic and scope.kind != ModuleScope:
      symbolMapper.symbolMapperError(
        node.pos, "Public functions are not allowed in inner scopes"
      )

    let functionScope = newScope(FunctionScope, scope, funDecl.identifier)

    # Create function symbol
    var paramTypes: seq[Type] = @[]
    for param in funDecl.parameters:
      paramTypes.add(param.paramType)

    let funSymbol = Symbol(
      name: funDecl.identifier,
      node: node,
      kind: Function,
      returnType: funDecl.returnType,
      paramTypes: paramTypes,
      scope: functionScope,
    )

    if not scope.addSymbol(funDecl.identifier, funSymbol):
      symbolMapper.symbolMapperError(
        node.pos, "Function '" & funDecl.identifier & "' already defined in this scope"
      )

    # Create function scope if body exists
    if funDecl.body.isSome:
      # Add parameters to function scope
      for param in funDecl.parameters:
        let paramSymbol = Symbol(
          name: param.name,
          node: node,
          kind: Variable,
          varType: param.paramType,
          isGlobal: false,
          isInitialized: true, # Parameters are initialized
        )

        if not functionScope.addSymbol(param.name, paramSymbol):
          symbolMapper.symbolMapperError(
            node.pos,
            "Parameter name '" & param.name & "' already used in this function",
          )

      initializeTable(symbolMapper, functionScope, funDecl.body.get)
  of SkBlockStmt:
    # Create block scope
    let blockScope = newScope(BlockScope, scope, node.blockStmt.blockId)
    for stmt in node.blockStmt.statements:
      initializeTable(symbolMapper, blockScope, stmt)
  of SkIfStmt:
    # Handle if statements with branches and optional else
    let ifStmt = node.ifStmt
    for branch in ifStmt.branches:
      let branchScope = newScope(BlockScope, scope, branch.scopeId)
      initializeTable(symbolMapper, branchScope, branch.body)
    if ifStmt.elseBranch.isSome:
      let elseBranch = ifStmt.elseBranch.get
      let elseScope = newScope(BlockScope, scope, elseBranch.scopeId)
      initializeTable(symbolMapper, elseScope, elseBranch.body)
  of SkExprStmt:
    # Expression statements cannot define new symbols
    discard
  of SkReturnStmt:
    # Return statements cannot define new symbols
    discard
  of SkNop:
    discard
