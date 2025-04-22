import ../types/[file_info, position, scope, ast, types]
import ../reporter
import std/options
import std/tables

type SymbolMapper* = ref object
  fileInfo*: FileInfo
  scope*: Scope
  hasError*: bool

proc symbolMapperError*(
    symbolMapper: SymbolMapper, pos: Position, msg: string, hint: string = ""
) =
  ## Logs an error during symbol mapping
  logError("SymbolMapper", pos, msg, hint)
  symbolMapper.hasError = true

proc newSymbolMapper*(fileInfo: FileInfo, scope: Scope): SymbolMapper =
  ## Creates a new symbol mapper for the given file
  result = SymbolMapper()
  result.fileInfo = fileInfo
  result.scope = scope
  result.hasError = false

proc astStructMembersToTypeMembers(
    members: Table[string, ast.StructMember]
): Table[string, types.StructMember] =
  ## Converts AST StructMember table to core StructMember table
  for name, astMember in members:
    result[name] = types.StructMember(
      name: astMember.name,
      typ: astMember.memberType,
      visibility: if astMember.isPublic: types.Public else: types.Private,
    )

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
    let varSymbol =
      Symbol(kind: Variable, declStmt: node, isInitialized: varDecl.initializer.isSome)
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

    let funSymbol = Symbol(kind: Function, declStmt: node, scope: functionScope)

    if not scope.addSymbol(funDecl.identifier, funSymbol):
      symbolMapper.symbolMapperError(
        node.pos, "Function '" & funDecl.identifier & "' already defined in this scope"
      )

    # Create function scope if body exists
    if funDecl.body.isSome:
      # Add parameters to function scope
      for param in funDecl.parameters:
        let paramSymbol = Symbol(
          kind: Variable,
          declStmt: Stmt(
            kind: SkVarDecl,
            pos: param.namePos,
            varDeclStmt: VarDeclStmt(
              identifier: param.name,
              typeAnnotation: param.paramType,
              initializer: param.defaultValue,
              isPublic: false, # Parameters are not public
            ),
          ),
          isInitialized: true, # Parameters are initialized
        )
        if not functionScope.addSymbol(param.name, paramSymbol):
          symbolMapper.symbolMapperError(
            node.pos,
            "Parameter name '" & param.name & "' already used in this function",
          )
      initializeTable(symbolMapper, functionScope, funDecl.body.get)
  of SkStructDecl:
    # Handle struct declarations
    let structDecl = node.structDeclStmt

    # do not allow public structs in inner scopes
    if structDecl.isPublic and scope.kind != ModuleScope:
      symbolMapper.symbolMapperError(
        node.pos, "Public structs are not allowed in inner scopes"
      )

    let structSymbol = Symbol(kind: Type, declStmt: node, typeRepr: nil)
    if not scope.addSymbol(structDecl.identifier, structSymbol):
      symbolMapper.symbolMapperError(
        node.pos, "Struct '" & structDecl.identifier & "' already defined in this scope"
      )
    structSymbol.typeRepr = Type(
      kind: TkStruct,
      structType: StructType(
        name: structSymbol.canonicalName,
        members: astStructMembersToTypeMembers(structDecl.members),
      ),
    )
  of SkTypeDecl:
    # Handle type declarations (aliases)
    let typeDecl = node.typeDeclStmt
    if typeDecl.isPublic and scope.kind != ModuleScope:
      symbolMapper.symbolMapperError(
        node.pos, "Public types are not allowed in inner scopes"
      )
    let typeSymbol =
      Symbol(kind: Type, declStmt: node, typeRepr: typeDecl.typeAnnotation)
    if not scope.addSymbol(typeDecl.identifier, typeSymbol):
      symbolMapper.symbolMapperError(
        node.pos, "Type '" & typeDecl.identifier & "' already defined in this scope"
      )
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
  of SkWhileStmt:
    # Handle while statements
    let whileStmt = node.whileStmt
    let whileScope = newScope(BlockScope, scope, whileStmt.scopeId)
    initializeTable(symbolMapper, whileScope, whileStmt.body)
  of SkExprStmt:
    # Expression statements cannot define new symbols
    discard
  of SkReturnStmt:
    # Return statements cannot define new symbols
    discard
  of SkNop:
    discard
