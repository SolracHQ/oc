import ../reporter
import
  ../types/[ast_c, ast, types, types_c, position, file_info, scope, annotation, token]
import ../semantic_analysis/analyzer
import std/tables
import std/algorithm
import std/sugar
import std/sequtils
import std/options
import std/strutils
import std/sets

type Transpiler* = ref object
  file: FileInfo
  hasError*: bool
  entryPoint: Option[Symbol] # Entry point for the transpiler, if any

proc transpilerError(
    transpiler: Transpiler, error: string, pos: Position, hint: string = ""
) =
  ## Log an error during transpilation
  logError("Transpiler", error, pos, hint)
  transpiler.hasError = true

proc transpileVariable(
  transpiler: Transpiler, scope: Scope, variable: Symbol, declarationOnly: bool = false
): Option[CNode]

proc makePointerType(base: CType): CType =
  result = CType(kind: CkPointer, ctype: new CType)
  result.ctype[] = base

proc getGlobalSymbols(transpiler: Transpiler, scope: Scope): seq[Symbol] =
  ## Get all global symbols from the scope
  for (name, symbol) in scope.symbols.pairs():
    if symbol.kind == Variable and symbol.isGlobal:
      result.add(symbol)
    elif symbol.kind == Function:
      result.add(symbol)
      if symbol.node.annotations.hasAnnotation("entrypoint"):
        if transpiler.entryPoint.isNone:
          transpiler.entryPoint = some(symbol)
        else:
          transpilerError(
            transpiler,
            "Multiple entry points found: " & symbol.canonicalName,
            symbol.node.pos,
          )
    elif symbol.kind == Type:
      result.add(symbol)
  for child in scope.children.values():
    result.add(getGlobalSymbols(transpiler, child))
  result.sort((s1, s2) => s1.kind.int.cmp s2.kind.int)

proc transpileNode(transpiler: Transpiler, scope: Scope, node: Node): Option[CNode] =
  case node.kind
  of NkModule:
    discard # handled at file level
  of NkVarDecl:
    let variableSymbol = scope.findSymbol(node.varDeclNode.identifier, node.pos)
    if variableSymbol.isNone:
      transpilerError(transpiler, "Cannot find variable declaration in scope", node.pos)
      return none(CNode)
    let variable = variableSymbol.get()
    result = transpiler.transpileVariable(scope, variable)
  of NkFunDecl:
    discard # all the functions should be handled at module level
  of NkBlockStmt:
    var stmts: seq[CNode] = @[]
    let blockScope = scope.children[node.blockStmtNode.blockId]
    for stmt in node.blockStmtNode.statements:
      let stmtC = transpileNode(transpiler, blockScope, stmt)
      if stmtC.isSome:
        stmts.add(stmtC.get())
    result = some(
      CNode(
        kind: CnkBlockStmt,
        pos: node.pos,
        comments: node.comments,
        blockStmtNode: ast_c.BlockStmtNode(statements: stmts),
      )
    )
  of NkExprStmt:
    let exprC = transpileNode(transpiler, scope, node.exprStmtNode.expression)
    if exprC.isSome:
      result = some(
        CNode(
          kind: CnkExprStmt,
          pos: node.pos,
          comments: node.comments,
          exprStmtNode: ast_c.ExprStmtNode(expression: exprC.get()),
        )
      )
  of NkReturnStmt:
    if node.returnStmtNode.expression.isSome:
      let exprC = transpileNode(transpiler, scope, node.returnStmtNode.expression.get())
      if exprC.isSome:
        result = some(
          CNode(
            kind: CnkReturnStmt,
            pos: node.pos,
            comments: node.comments,
            returnStmtNode: ast_c.ReturnStmtNode(expression: exprC),
          )
        )
    else:
      result = some(
        CNode(
          kind: CnkReturnStmt,
          pos: node.pos,
          comments: node.comments,
          returnStmtNode: ast_c.ReturnStmtNode(expression: none(CNode)),
        )
      )
  of NkAssignment:
    let lhsSym = scope.findSymbol(node.assignmentNode.identifier, node.pos)
    if lhsSym.isNone:
      transpilerError(
        transpiler,
        "Assignment to undeclared variable: " & node.assignmentNode.identifier,
        node.pos,
      )
      return none(CNode)
    let lhs = CNode(
      kind: CnkIdentifier,
      pos: node.pos,
      comments: node.comments,
      identifierNode: ast_c.IdentifierNode(name: lhsSym.get().canonicalName),
    )
    let rhsC = transpileNode(transpiler, scope, node.assignmentNode.value)
    if rhsC.isNone:
      return none(CNode)
    result = some(
      CNode(
        kind: CnkAssignment,
        pos: node.pos,
        comments: node.comments,
        assignmentNode:
          ast_c.AssignmentNode(lhs: lhs, rhs: rhsC.get(), operator: TkEqual),
      )
    )
  of NkLogicalExpr, NkEqualityExpr, NkComparisonExpr, NkAdditiveExpr,
      NkMultiplicativeExpr:
    let leftC = transpileNode(transpiler, scope, node.binaryOpNode.left)
    let rightC = transpileNode(transpiler, scope, node.binaryOpNode.right)
    if leftC.isNone or rightC.isNone:
      return none(CNode)
    result = some(
      CNode(
        kind: CnkBinaryExpr,
        pos: node.pos,
        comments: node.comments,
        binaryExprNode: BinaryExprNode(
          left: leftC.get(), operator: node.binaryOpNode.operator, right: rightC.get()
        ),
      )
    )
  of NkUnaryExpr:
    let operandC = transpileNode(transpiler, scope, node.unaryOpNode.operand)
    if operandC.isNone:
      return none(CNode)
    result = some(
      CNode(
        kind: CnkUnaryExpr,
        pos: node.pos,
        comments: node.comments,
        unaryExprNode:
          UnaryExprNode(operator: node.unaryOpNode.operator, operand: operandC.get()),
      )
    )
  of NkMemberAccess:
    let objC = transpileNode(transpiler, scope, node.memberAccessNode.obj)
    if objC.isNone:
      return none(CNode)
    # For now, treat as identifier access: obj.member -> obj_member (flattened)
    let flatName = objC.get().identifierNode.name & "_" & node.memberAccessNode.member
    result = some(
      CNode(
        kind: CnkIdentifier,
        pos: node.pos,
        comments: node.comments,
        identifierNode: ast_c.IdentifierNode(name: flatName),
      )
    )
  of NkFunctionCall:
    let calleeC = transpileNode(transpiler, scope, node.functionCallNode.callee)
    if calleeC.isNone:
      return none(CNode)
    var argsC: seq[CNode] = @[]
    for arg in node.functionCallNode.arguments:
      let argC = transpileNode(transpiler, scope, arg)
      if argC.isNone:
        return none(CNode)
      argsC.add(argC.get())
    result = some(
      CNode(
        kind: CnkFunctionCall,
        pos: node.pos,
        comments: node.comments,
        functionCallNode:
          ast_c.FunctionCallNode(callee: calleeC.get(), arguments: argsC),
      )
    )
  of NkIdentifier:
    let symOpt = scope.findSymbol(node.identifierNode.name, node.pos)
    if symOpt.isNone:
      transpilerError(
        transpiler, "Unknown identifier: " & node.identifierNode.name, node.pos
      )
      return none(CNode)
    result = some(
      CNode(
        kind: CnkIdentifier,
        pos: node.pos,
        comments: node.comments,
        identifierNode: ast_c.IdentifierNode(name: symOpt.get().canonicalName),
      )
    )
  of NkGroupExpr:
    let exprC = transpileNode(transpiler, scope, node.groupNode.expression)
    if exprC.isSome:
      result = some(
        CNode(
          kind: CnkGroupExpr,
          pos: node.pos,
          comments: node.comments,
          groupNode: ast_c.GroupNode(expression: exprC.get()),
        )
      )
  of NkIntLiteral:
    result = some(
      CNode(
        kind: CnkIntLiteral,
        pos: node.pos,
        comments: node.comments,
        intLiteralNode: ast_c.IntLiteralNode(value: node.intLiteralNode.value),
      )
    )
  of NkUIntLiteral:
    result = some(
      CNode(
        kind: CnkUIntLiteral,
        pos: node.pos,
        comments: node.comments,
        uintLiteralNode: ast_c.UIntLiteralNode(value: node.uintLiteralNode.value),
      )
    )
  of NkFloatLiteral:
    result = some(
      CNode(
        kind: CnkFloatLiteral,
        pos: node.pos,
        comments: node.comments,
        floatLiteralNode: ast_c.FloatLiteralNode(value: node.floatLiteralNode.value),
      )
    )
  of NkStringLiteral:
    discard #todo when we got structs
  of NkCStringLiteral:
    result = some(
      CNode(
        kind: CnkStringLiteral,
        pos: node.pos,
        comments: node.comments,
        stringLiteralNode: ast_c.StringLiteralNode(value: node.cStringLiteralNode.value),
      )
    )
  of NkCharLiteral:
    result = some(
      CNode(
        kind: CnkCharLiteral,
        pos: node.pos,
        comments: node.comments,
        charLiteralNode: ast_c.CharLiteralNode(value: node.charLiteralNode.value),
      )
    )
  of NkBoolLiteral:
    result = some(
      CNode(
        kind: CnkBoolLiteral,
        pos: node.pos,
        comments: node.comments,
        boolLiteralNode: ast_c.BoolLiteralNode(value: node.boolLiteralNode.value),
      )
    )
  of NkNilLiteral:
    result = some(CNode(kind: CnkNullLiteral, pos: node.pos, comments: node.comments))
  of NkType, NkNop:
    discard # Types and nops are not emitted

proc transpileFunction(
    transpiler: Transpiler,
    scope: Scope,
    function: Symbol,
    declarationOnly: bool = false,
): Option[CNode] =
  if function.node.annotations["include"].isSome() and
      function.paramTypes.anyIt(it.kind == TkMeta and it.metaKind == MkCVarArgs):
    return
      CNode(
        kind: CnkDefine,
        pos: function.node.pos,
        defineNode: DefineNode(
          name: function.canonicalName,
          value:
            CNode(
              kind: CnkIdentifier,
              identifierNode: ast_c.IdentifierNode(
                name: function.node.annotations["include", "name"].get()
              ),
            ).some,
        ),
      ).some
  let name = function.canonicalName
  let returnType = function.returnType.toCType()
  if returnType.isNone:
    transpilerError(
      transpiler,
      "Invalid return type for function: " & name,
      function.node.pos,
      "Metatypes cannot be mapped to C types",
    )
    return none(CNode)
  let returnTypeC: CType = returnType.get()
  let isStatic = function.node.annotations["static"].isSome()
  let isExtern = function.node.annotations["extern"].isSome()
  let isInline = function.node.annotations["inline"].isSome()
  var parameters: seq[ast_c.ParameterNode] = @[]
  for parameter in function.node.funDeclNode.parameters:
    let paramType = parameter.paramType.toCType()
    if paramType.isNone:
      transpilerError(
        transpiler,
        "Invalid parameter type for function: " & name,
        function.node.pos,
        "Metatypes cannot be mapped to C types",
      )
      return none(CNode)
    let paramTypeC: CType = paramType.get()
    parameters.add(
      ast_c.ParameterNode(name: parameter.identifier, paramType: paramTypeC)
    )
  if declarationOnly or isExtern:
    return
      CNode(
        kind: CnkFunctionDecl,
        functionDeclNode: FunctionDeclNode(
          name: name,
          returnType: returnTypeC,
          parameters: parameters,
          isStatic: isStatic,
          isExtern: isExtern,
          isInline: isInline,
        ),
      ).some
  let body = function.node.funDeclNode.body
  if body.isNone:
    transpilerError(
      transpiler,
      "Function body is None for function: " & name,
      function.node.pos,
      "Function must have a body",
    )
    return none(CNode)

  let bodyNode = transpileNode(transpiler, scope, body.get())
  if bodyNode.isNone:
    if function.node.annotations["include"].isNone:
      transpilerError(
        transpiler,
        "Invalid function body for function: " & name,
        function.node.pos,
        "Function body must be a block statement",
      )
      return none(CNode)
    else:
      return
        CNode(
          kind: CnkReturnStmt,
          returnStmtNode: ast_c.ReturnStmtNode(
            expression:
              CNode(
                kind: CnkFunctionCall,
                functionCallNode: ast_c.FunctionCallNode(
                  callee: CNode(
                    kind: CnkIdentifier,
                    identifierNode: ast_c.IdentifierNode(name: name),
                  ),
                  arguments: function.node.funDeclNode.parameters.map(
                    (it) =>
                      CNode(
                        kind: CnkIdentifier,
                        identifierNode: ast_c.IdentifierNode(name: it.identifier),
                      )
                  ),
                ),
              ).some
          ),
        ).some
  let bodyC: CNode = bodyNode.get()
  return
    CNode(
      kind: CnkFunctionDef,
      pos: function.node.pos,
      functionDefNode: FunctionDefNode(
        declaration: CNode(
          kind: CnkFunctionDecl,
          functionDeclNode: FunctionDeclNode(
            name: name,
            returnType: returnTypeC,
            parameters: parameters,
            isStatic: isStatic,
            isExtern: isExtern,
            isInline: isInline,
          ),
        ),
        body: bodyC,
      ),
    ).some

proc transpileVariable(
    transpiler: Transpiler,
    scope: Scope,
    variable: Symbol,
    declarationOnly: bool = false,
): Option[CNode] =
  let name = variable.canonicalName
  let varType = variable.varType.toCType()
  if varType.isNone:
    transpilerError(
      transpiler,
      "Invalid variable type for variable: " & name,
      variable.node.pos,
      "Metatypes cannot be mapped to C types",
    )
    return none(CNode)
  let varTypeC: CType = varType.get()
  let isStatic = variable.node.annotations["static"].isSome()
  let isExtern = variable.node.annotations["extern"].isSome()
  let isVolatile = variable.node.annotations["volatile"].isSome()
  let isConst = variable.node.varDeclNode.isReadOnly
  let initializer =
    if declarationOnly or isExtern:
      none(CNode)
    else:
      let init = variable.node.varDeclNode.initializer
      if init.isNone:
        transpilerError(
          transpiler,
          "Variable initializer is None for variable: " & name,
          variable.node.pos,
          "Variable must have an initializer",
        )
        return none(CNode)
      let initNode = transpileNode(transpiler, scope, init.get())
      if initNode.isNone:
        return none(CNode)
      initNode
  return
    CNode(
      kind: CnkVarDecl,
      pos: variable.node.pos,
      varDeclNode: ast_c.VarDeclNode(
        name: name,
        varType: varTypeC,
        initializer: initializer,
        isStatic: isStatic,
        isExtern: isExtern,
        isVolatile: isVolatile,
        isConst: isConst,
      ),
    ).some

proc transpileHFile(transpiler: Transpiler, globalSymbols: seq[Symbol]): Option[CNode] =
  var moduleH = CNode(
    kind: CNodeKind.CnkTranslationUnit,
    translationUnitNode: TranslationUnitNode(
      includes: [
        IncludeNode(isSystem: true, file: "<stdint.h>"),
        IncludeNode(isSystem: true, file: "<stdbool.h>"),
      ].toHashSet()
    ),
  )
  for symbol in globalSymbols:
    if (symbol.kind == Function and not symbol.node.funDeclNode.isPublic) or
        (symbol.kind == Variable and not symbol.node.varDeclNode.isPublic):
      # Only include public functions and variables
      continue
    let headerOpt = symbol.node.annotations["include", "from"]
    if headerOpt.isSome() and symbol.kind == Function and
        symbol.paramTypes[^1].kind == TkMeta and
        symbol.paramTypes[^1].metaKind == MkCVarArgs:
      # Only include if it's a varargs function, otherwise it's included in the C file
      let header: string = headerOpt.get()
      let isSystem = header.startsWith("<") and header.endsWith(">")
      moduleH.translationUnitNode.includes.incl(
        IncludeNode(isSystem: isSystem, file: header)
      )
    if symbol.kind == Function:
      let function = transpileFunction(transpiler, symbol.scope, symbol, true)
      if function.isNone:
        return none(CNode)
      moduleH.translationUnitNode.declarations.add(function.get())
    elif symbol.kind == Variable:
      let variable = transpileVariable(transpiler, nil, symbol, true)
      if variable.isNone:
        return none(CNode)
      moduleH.translationUnitNode.declarations.add(variable.get())
    else:
      transpilerError(
        transpiler,
        "Invalid symbol kind for header: " & symbol.canonicalName,
        symbol.node.pos,
        "Remember to implement " & $symbol.kind,
      )
      return none(CNode)
  result =
    CNode(
      kind: CnkIfNotDef,
      ifNotDefNode: ast_c.IfNotDefNode(
        name: transpiler.file.name.toUpperAscii() & "_H",
        body:
          @[
            CNode(
              kind: CnkDefine,
              defineNode: ast_c.DefineNode(
                name: transpiler.file.name.toUpperAscii() & "_H", value: none(CNode)
              ),
            ), moduleH,
          ],
        elseBody: @[],
      ),
    ).some

proc generateMainFunction(transpiler: Transpiler): Option[CNode] =
  ## Generate a C main function that calls the entry point
  if transpiler.entryPoint.isNone:
    return none(CNode)
  let entry = transpiler.entryPoint.get()
  let entryName = entry.canonicalName
  let paramTypes = entry.paramTypes
  let returnTypeOpt = entry.returnType.toCType()
  if returnTypeOpt.isNone:
    transpilerError(
      transpiler, "Entry point return type cannot be mapped to C", entry.node.pos
    )
    return none(CNode)
  let returnType = returnTypeOpt.get()
  # Only allow 0, 2, or 3 parameters
  if not (paramTypes.len == 0 or paramTypes.len == 2 or paramTypes.len == 3):
    transpilerError(
      transpiler, "Entry point must have 0, 2, or 3 parameters", entry.node.pos
    )
    return none(CNode)
  # Only allow return type int or void
  let isInt =
    returnType.kind == CkPrimitive and
    (returnType.primitive == Int64T or returnType.primitive == Int32T)
  let isVoid = returnType.kind == CkPrimitive and returnType.primitive == VoidT
  if not (isInt or isVoid):
    transpilerError(transpiler, "Entry point must return Int or Void", entry.node.pos)
    return none(CNode)
  # Build parameters for main
  var mainParams: seq[ast_c.ParameterNode] = @[]
  var callArgs: seq[CNode] = @[]
  let charPtr = CType(kind: CkPrimitive, primitive: CharPtrT)
  let charPtrPtr = makePointerType(charPtr)
  let int32T = CType(kind: CkPrimitive, primitive: Int32T)
  if paramTypes.len == 0:
    discard
  elif paramTypes.len == 2:
    # int argc, char** argv
    mainParams.add(ast_c.ParameterNode(name: "argc", paramType: int32T))
    mainParams.add(ast_c.ParameterNode(name: "argv", paramType: charPtrPtr))
    callArgs.add(
      CNode(kind: CnkIdentifier, identifierNode: ast_c.IdentifierNode(name: "argc"))
    )
    callArgs.add(
      CNode(kind: CnkIdentifier, identifierNode: ast_c.IdentifierNode(name: "argv"))
    )
  elif paramTypes.len == 3:
    # int argc, char** argv, char** envp
    mainParams.add(ast_c.ParameterNode(name: "argc", paramType: int32T))
    mainParams.add(ast_c.ParameterNode(name: "argv", paramType: charPtrPtr))
    mainParams.add(ast_c.ParameterNode(name: "envp", paramType: charPtrPtr))
    callArgs.add(
      CNode(kind: CnkIdentifier, identifierNode: ast_c.IdentifierNode(name: "argc"))
    )
    callArgs.add(
      CNode(kind: CnkIdentifier, identifierNode: ast_c.IdentifierNode(name: "argv"))
    )
    callArgs.add(
      CNode(kind: CnkIdentifier, identifierNode: ast_c.IdentifierNode(name: "envp"))
    )
  # Build call to entry point
  let callEntry = CNode(
    kind: CnkFunctionCall,
    functionCallNode: ast_c.FunctionCallNode(
      callee: CNode(
        kind: CnkIdentifier, identifierNode: ast_c.IdentifierNode(name: entryName)
      ),
      arguments: callArgs,
    ),
  )
  var stmts: seq[CNode] = @[]
  if isInt:
    stmts.add(
      CNode(
        kind: CnkReturnStmt,
        returnStmtNode: ast_c.ReturnStmtNode(expression: some(callEntry)),
      )
    )
  else:
    stmts.add(
      CNode(kind: CnkExprStmt, exprStmtNode: ast_c.ExprStmtNode(expression: callEntry))
    )
    stmts.add(
      CNode(
        kind: CnkReturnStmt,
        returnStmtNode: ast_c.ReturnStmtNode(
          expression:
            CNode(kind: CnkIntLiteral, intLiteralNode: ast_c.IntLiteralNode(value: 0)).some
        ),
      )
    )
  let mainDecl = CNode(
    kind: CnkFunctionDecl,
    functionDeclNode: ast_c.FunctionDeclNode(
      name: "main",
      returnType: int32T,
      parameters: mainParams,
      isStatic: false,
      isExtern: false,
      isInline: false,
    ),
  )
  let mainBody =
    CNode(kind: CnkBlockStmt, blockStmtNode: ast_c.BlockStmtNode(statements: stmts))
  return
    CNode(
      kind: CnkFunctionDef,
      functionDefNode: ast_c.FunctionDefNode(declaration: mainDecl, body: mainBody),
    ).some

proc transpileCFile(
    transpiler: Transpiler, globalSymbols: seq[Symbol], scope: Scope, node: Node
): Option[CNode] =
  var moduleC = CNode(
    kind: CnkTranslationUnit,
    translationUnitNode: TranslationUnitNode(
      includes:
        [IncludeNode(isSystem: false, file: transpiler.file.name & ".h")].toHashSet()
    ),
  )
  # Add declarations and includes to the C file
  for symbol in globalSymbols:
    if (symbol.kind == Function and symbol.node.funDeclNode.isPublic) or
        (symbol.kind == Variable and symbol.node.varDeclNode.isPublic):
      continue
    let headerOpt = symbol.node.annotations["include", "from"]
    if headerOpt.isSome() and symbol.kind == Function and
        symbol.paramTypes[^1].kind == TkMeta and
        symbol.paramTypes[^1].metaKind == MkCVarArgs:
      # Only include if it's a varargs function, otherwise it's included in the C file
      let header: string = headerOpt.get()
      let isSystem = header.startsWith("<") and header.endsWith(">")
      moduleC.translationUnitNode.includes.incl(
        IncludeNode(isSystem: isSystem, file: header)
      )
    if symbol.kind == Function:
      let function = transpileFunction(transpiler, symbol.scope, symbol, true)
      if function.isNone:
        return none(CNode)
      moduleC.translationUnitNode.declarations.add(function.get())
    elif symbol.kind == Variable:
      let variable = transpileVariable(transpiler, scope, symbol, true)
      if variable.isNone:
        return none(CNode)
      moduleC.translationUnitNode.declarations.add(variable.get())
    else:
      transpilerError(
        transpiler,
        "Invalid symbol kind for c file: " & symbol.canonicalName,
        symbol.node.pos,
        "Remember to implement " & $symbol.kind,
      )
      return none(CNode)

  # Add implementations to the C file
  for symbol in globalSymbols:
    if symbol.kind == Function:
      let function = transpileFunction(transpiler, symbol.scope, symbol)
      if function.isNone:
        return none(CNode)
      moduleC.translationUnitNode.declarations.add(function.get())
    elif symbol.kind == Variable:
      let variable = transpileVariable(transpiler, scope, symbol)
      if variable.isNone:
        return none(CNode)
      moduleC.translationUnitNode.declarations.add(variable.get())
    else:
      transpilerError(
        transpiler,
        "Invalid symbol kind for c file: " & symbol.canonicalName,
        symbol.node.pos,
        "Remember to implement " & $symbol.kind,
      )
      return none(CNode)

  # Add the main function if entry point is set
  let mainFunc = generateMainFunction(transpiler)
  if mainFunc.isSome:
    moduleC.translationUnitNode.declarations.add(mainFunc.get())

  result = moduleC.some

proc transpile*(file: FileInfo): tuple[hasError: bool, hFile: CNode, cFile: CNode] =
  let analyzer = newAnalyzer(file)
  let (hasError, scope, ast) = analyzer.analyze()
  let transpiler = Transpiler(file: file, hasError: hasError)
  if hasError:
    return (true, nil, nil)
  let globalSymbols = transpiler.getGlobalSymbols(scope)
  let hFile = transpileHFile(transpiler, globalSymbols)
  if hFile.isNone:
    return (true, nil, nil)
  let cFile = transpileCFile(transpiler, globalSymbols, scope, ast)
  if cFile.isNone:
    return (true, nil, nil)
  return (false, hFile.get(), cFile.get())
