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

proc initializeTable*(symbolMapper: SymbolMapper, scope: Scope, node: Node) =
  ## Initializes the symbol table for the analyzer
  case node.kind
  of NkModule:
    # Process each statement in the module
    for stmt in node.moduleNode.statements:
      initializeTable(symbolMapper, scope, stmt)
  of NkVarDecl:
    # Handle variable declarations
    let varDecl = node.varDeclNode
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
      initializeTable(symbolMapper, scope, varDecl.initializer.get)
  of NkFunDecl:
    # Handle function declarations
    let funDecl = node.funDeclNode
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
          name: param.identifier,
          node: node,
          kind: Variable,
          varType: param.paramType,
          isGlobal: false,
          isInitialized: true, # Parameters are initialized
        )

        if not functionScope.addSymbol(param.identifier, paramSymbol):
          symbolMapper.symbolMapperError(
            node.pos,
            "Parameter name '" & param.identifier & "' already used in this function",
          )

      initializeTable(symbolMapper, functionScope, funDecl.body.get)
  of NkBlockStmt:
    # Create block scope
    let blockScope = newScope(BlockScope, scope, node.blockStmtNode.blockId)
    for stmt in node.blockStmtNode.statements:
      initializeTable(symbolMapper, blockScope, stmt)
      
  of NkExprStmt:
    # Expression statements cannot define new symbols
    discard
  of NkReturnStmt:
    # Return statements cannot define new symbols
    discard
  of NkAssignment, NkLogicalExpr, NkEqualityExpr, NkComparisonExpr, NkAdditiveExpr,
      NkMultiplicativeExpr, NkUnaryExpr, NkMemberAccess, NkFunctionCall, NkGroupExpr:
    # Only statements can define new symbols
    discard

  # Leaf nodes - no further processing needed
  of NkIdentifier, NkIntLiteral, NkUIntLiteral, NkFloatLiteral, NkStringLiteral, NkCStringLiteral, NkCharLiteral,
      NkBoolLiteral, NkNilLiteral, NkType, NkNop:
    discard