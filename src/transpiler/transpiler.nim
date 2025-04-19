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
): Option[CStmt]

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

proc transpileExpr(transpiler: Transpiler, scope: Scope, expr: Expr): Option[CExpr] =
  case expr.kind
  of EkAssignment:
    let lhsSym = scope.findSymbol(expr.assignmentExpr.identifier, expr.pos, Variable)
    if lhsSym.isNone:
      transpilerError(
        transpiler,
        "Assignment to undeclared variable: " & expr.assignmentExpr.identifier,
        expr.pos,
      )
      return none(CExpr)
    let lhs = CExpr(
      kind: CekIdentifier,
      pos: expr.pos,
      identifierNode: ast_c.IdentifierNode(name: lhsSym.get().canonicalName),
    )
    let rhsC = transpileExpr(transpiler, scope, expr.assignmentExpr.value)
    if rhsC.isNone:
      return none(CExpr)
    result = some(
      CExpr(
        kind: CekAssignment,
        pos: expr.pos,
        assignmentNode:
          ast_c.AssignmentNode(lhs: lhs, rhs: rhsC.get(), operator: TkEqual),
      )
    )
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr, EkMultiplicativeExpr:
    let leftC = transpileExpr(transpiler, scope, expr.binaryOpExpr.left)
    let rightC = transpileExpr(transpiler, scope, expr.binaryOpExpr.right)
    if leftC.isNone or rightC.isNone:
      return none(CExpr)
    result = some(
      CExpr(
        kind: CekBinaryExpr,
        pos: expr.pos,
        
        binaryExprNode: ast_c.BinaryExprNode(
          left: leftC.get(), operator: expr.binaryOpExpr.operator, right: rightC.get()
        ),
      )
    )
  of EkUnaryExpr:
    let operandC = transpileExpr(transpiler, scope, expr.unaryOpExpr.operand)
    if operandC.isNone:
      return none(CExpr)
    result = some(
      CExpr(
        kind: CekUnaryExpr,
        pos: expr.pos,
        
        unaryExprNode:
          ast_c.UnaryExprNode(operator: expr.unaryOpExpr.operator, operand: operandC.get()),
      )
    )
  of EkMemberAccess:
    let objC = transpileExpr(transpiler, scope, expr.memberAccessExpr.obj)
    if objC.isNone:
      return none(CExpr)
    let flatName = objC.get().identifierNode.name & "_" & expr.memberAccessExpr.member
    result = some(
      CExpr(
        kind: CekIdentifier,
        pos: expr.pos,
        
        identifierNode: ast_c.IdentifierNode(name: flatName),
      )
    )
  of EkFunctionCall:
    let calleeC = transpileExpr(transpiler, scope, expr.functionCallExpr.callee)
    if calleeC.isNone:
      return none(CExpr)
    var argsC: seq[CExpr] = @[]
    for arg in expr.functionCallExpr.arguments:
      let argC = transpileExpr(transpiler, scope, arg)
      if argC.isNone:
        return none(CExpr)
      argsC.add(argC.get())
    result = some(
      CExpr(
        kind: CekFunctionCall,
        pos: expr.pos,
        
        functionCallNode:
          ast_c.FunctionCallNode(callee: calleeC.get(), arguments: argsC),
      )
    )
  of EkIdentifier:
    let symOpt = scope.findSymbol(expr.identifierExpr.name, expr.pos, AnySymbol)
    if symOpt.isNone:
      transpilerError(
        transpiler, "Unknown identifier: " & expr.identifierExpr.name, expr.pos
      )
      return none(CExpr)
    result = some(
      CExpr(
        kind: CekIdentifier,
        pos: expr.pos,
        
        identifierNode: ast_c.IdentifierNode(name: symOpt.get().canonicalName),
      )
    )
  of EkGroupExpr:
    let exprC = transpileExpr(transpiler, scope, expr.groupExpr.expression)
    if exprC.isSome:
      result = some(
        CExpr(
          kind: CekGroupExpr,
          pos: expr.pos,
          
          groupNode: ast_c.GroupNode(expression: exprC.get()),
        )
      )
  of EkAddressOfExpr:
    let operandC = transpileExpr(transpiler, scope, expr.addressOfExpr.operand)
    if operandC.isNone:
      return none(CExpr)
    result = some(
      CExpr(
        kind: CekAddressOf,
        pos: expr.pos,
        
        addressOfNode: ast_c.AddressOfNode(operand: operandC.get()),
      )
    )
  of EkDerefExpr:
    let operandC = transpileExpr(transpiler, scope, expr.derefExpr.operand)
    if operandC.isNone:
      return none(CExpr)
    result = some(
      CExpr(
        kind: CekDereference,
        pos: expr.pos,
        
        dereferenceNode: ast_c.DereferenceNode(operand: operandC.get()),
      )
    )
  of EkIntLiteral:
    result = some(
      CExpr(
        kind: CekIntLiteral,
        pos: expr.pos,
        
        intLiteralNode: ast_c.IntLiteralNode(value: expr.intLiteralExpr.value),
      )
    )
  of EkUIntLiteral:
    result = some(
      CExpr(
        kind: CekUIntLiteral,
        pos: expr.pos,
        
        uintLiteralNode: ast_c.UIntLiteralNode(value: expr.uintLiteralExpr.value),
      )
    )
  of EkFloatLiteral:
    result = some(
      CExpr(
        kind: CekFloatLiteral,
        pos: expr.pos,
        
        floatLiteralNode: ast_c.FloatLiteralNode(value: expr.floatLiteralExpr.value),
      )
    )
  of EkStringLiteral:
    discard # TODO: handle string literals
  of EkCStringLiteral:
    result = some(
      CExpr(
        kind: CekStringLiteral,
        pos: expr.pos,
        
        stringLiteralNode: ast_c.StringLiteralNode(value: expr.cStringLiteralExpr.value),
      )
    )
  of EkCharLiteral:
    result = some(
      CExpr(
        kind: CekCharLiteral,
        pos: expr.pos,
        
        charLiteralNode: ast_c.CharLiteralNode(value: expr.charLiteralExpr.value),
      )
    )
  of EkBoolLiteral:
    result = some(
      CExpr(
        kind: CekBoolLiteral,
        pos: expr.pos,
        
        boolLiteralNode: ast_c.BoolLiteralNode(value: expr.boolLiteralExpr.value),
      )
    )
  of EkNilLiteral:
    result = some(CExpr(kind: CekNullLiteral, pos: expr.pos))
  of EkType:
    transpiler.transpilerError(
      "Type are not values in OverC", expr.pos, "Cannot use type as value"
    )
    return none(CExpr)

proc transpileStmt(transpiler: Transpiler, scope: Scope, stmt: Stmt): Option[CStmt] =
  case stmt.kind
  of SkModule:
    discard # handled at file level
  of SkVarDecl:
    let variableSymbol = scope.findSymbol(stmt.varDeclStmt.identifier, stmt.pos, Variable)
    if variableSymbol.isNone:
      transpilerError(transpiler, "Cannot find variable declaration in scope", stmt.pos)
      return none(CStmt)
    let variable = variableSymbol.get()
    result = transpiler.transpileVariable(scope, variable)
  of SkFunDecl:
    discard # all the functions should be handled at module level
  of SkBlockStmt:
    var stmts: seq[CStmt] = @[]
    let blockScope = scope.children[stmt.blockStmt.blockId]
    for s in stmt.blockStmt.statements:
      let stmtC = transpileStmt(transpiler, blockScope, s)
      if stmtC.isSome:
        stmts.add(stmtC.get())
    result = some(
      CStmt(
        kind: CskBlockStmt,
        pos: stmt.pos,
        comments: stmt.comments,
        blockStmtNode: ast_c.BlockStmtNode(statements: stmts),
      )
    )
  of SkExprStmt:
    let exprC = transpileExpr(transpiler, scope, stmt.exprStmt.expression)
    if exprC.isSome:
      result = some(
        CStmt(
          kind: CskExprStmt,
          pos: stmt.pos,
          comments: stmt.comments,
          exprStmtNode: ast_c.ExprStmtNode(expression: exprC.get()),
        )
      )
  of SkReturnStmt:
    if stmt.returnStmt.expression.isSome:
      let exprC = transpileExpr(transpiler, scope, stmt.returnStmt.expression.get())
      if exprC.isSome:
        result = some(
          CStmt(
            kind: CskReturnStmt,
            pos: stmt.pos,
            comments: stmt.comments,
            returnStmtNode: ast_c.ReturnStmtNode(expression: some(exprC.get())),
          )
        )
    else:
      result = some(
        CStmt(
          kind: CskReturnStmt,
          pos: stmt.pos,
          comments: stmt.comments,
          returnStmtNode: ast_c.ReturnStmtNode(expression: none(CExpr)),
        )
      )
  of SkIfStmt:
    var cBranches: seq[ast_c.IfBranchNode] = @[]
    for branch in stmt.ifStmt.branches:
      let branchScope = scope.children[branch.scopeId]
      let condC = transpileExpr(transpiler, branchScope, branch.condition)
      let bodyC = transpileStmt(transpiler, branchScope, branch.body)
      if condC.isNone or bodyC.isNone:
        return none(CStmt)
      cBranches.add(ast_c.IfBranchNode(condition: condC.get(), body: bodyC.get()))
    var cElse: Option[CStmt] = none(CStmt)
    if stmt.ifStmt.elseBranch.isSome:
      let elseB = stmt.ifStmt.elseBranch.get()
      let elseScope = scope.children[elseB.scopeId]
      let elseBodyC = transpileStmt(transpiler, elseScope, elseB.body)
      if elseBodyC.isNone:
        return none(CStmt)
      cElse = some(elseBodyC.get())
    result = some(
      CStmt(
        kind: CskIfStmt,
        pos: stmt.pos,
        comments: stmt.comments,
        ifStmtNode: ast_c.IfStmtNode(branches: cBranches, elseBranch: cElse),
      )
    )
  of SkNop:
    return none(CStmt)

proc transpileFunction(
    transpiler: Transpiler,
    scope: Scope,
    function: Symbol,
    declarationOnly: bool = false,
): Option[CStmt] =
  if function.node.annotations["include"].isSome() and
      function.paramTypes.anyIt(it.kind == TkMeta and it.metaKind == MkCVarArgs):
    return
      CStmt(
        kind: CskDefine,
        pos: function.node.pos,
        comments: function.node.comments,
        defineNode: ast_c.DefineNode(
          name: function.canonicalName,
          value:
            CExpr(
              kind: CekIdentifier,
              pos: function.node.pos,
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
    return none(CStmt)
  let returnTypeC: CType = returnType.get()
  let isStatic = function.node.annotations["static"].isSome()
  let isExtern = function.node.annotations["extern"].isSome()
  let isInline = function.node.annotations["inline"].isSome()
  var parameters: seq[ast_c.ParameterNode] = @[]
  for parameter in function.node.funDeclStmt.parameters:
    let paramType = parameter.paramType.toCType()
    if paramType.isNone:
      transpilerError(
        transpiler,
        "Invalid parameter type for function: " & name,
        function.node.pos,
        "Metatypes cannot be mapped to C types",
      )
      return none(CStmt)
    let paramTypeC: CType = paramType.get()
    parameters.add(
      ast_c.ParameterNode(name: parameter.name, paramType: paramTypeC)
    )
  if declarationOnly or isExtern:
    return
      CStmt(
        kind: CskFunctionDecl,
        pos: function.node.pos,
        comments: function.node.comments,
        functionDeclNode: ast_c.FunctionDeclNode(
          name: name,
          returnType: returnTypeC,
          parameters: parameters,
          isStatic: isStatic,
          isExtern: isExtern,
          isInline: isInline,
        ),
      ).some
  let body = function.node.funDeclStmt.body
  if body.isNone:
    transpilerError(
      transpiler,
      "Function body is None for function: " & name,
      function.node.pos,
      "Function must have a body",
    )
    return none(CStmt)

  let bodyNode = transpileStmt(transpiler, scope, body.get())
  if bodyNode.isNone:
    if function.node.annotations["include"].isNone:
      transpilerError(
        transpiler,
        "Invalid function body for function: " & name,
        function.node.pos,
        "Function body must be a block statement",
      )
      return none(CStmt)
    else:
      return
        CStmt(
          kind: CskReturnStmt,
          pos: function.node.pos,
          comments: function.node.comments,
          returnStmtNode: ast_c.ReturnStmtNode(
            expression:
              CExpr(
                kind: CekFunctionCall,
                pos: function.node.pos,
                functionCallNode: ast_c.FunctionCallNode(
                  callee: CExpr(
                    kind: CekIdentifier,
                    pos: function.node.pos,
                    identifierNode: ast_c.IdentifierNode(name: name),
                  ),
                  arguments: function.node.funDeclStmt.parameters.map(
                    (it) =>
                      CExpr(
                        kind: CekIdentifier,
                        pos: function.node.pos,
                        identifierNode: ast_c.IdentifierNode(name: it.name),
                      )
                  ),
                ),
              ).some
          ),
        ).some
  let bodyC: CStmt = bodyNode.get()
  return
    CStmt(
      kind: CskFunctionDef,
      pos: function.node.pos,
      comments: function.node.comments,
      functionDefNode: ast_c.FunctionDefNode(
        declaration: CStmt(
          kind: CskFunctionDecl,
          pos: function.node.pos,
          comments: function.node.comments,
          functionDeclNode: ast_c.FunctionDeclNode(
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
): Option[CStmt] =
  let name = variable.canonicalName
  let varType = variable.varType.toCType()
  if varType.isNone:
    transpilerError(
      transpiler,
      "Invalid variable type for variable: " & name,
      variable.node.pos,
      "Metatypes cannot be mapped to C types",
    )
    return none(CStmt)
  let varTypeC: CType = varType.get()
  let isStatic = variable.node.annotations["static"].isSome()
  let isExtern = variable.node.annotations["extern"].isSome()
  let isVolatile = variable.node.annotations["volatile"].isSome()
  let isConst = variable.node.varDeclStmt.isReadOnly
  let initializer =
    if declarationOnly or isExtern or variable.node.varDeclStmt.initializer.isNone:
      none(CExpr)
    else:
      let init = variable.node.varDeclStmt.initializer
      let initNode = transpileExpr(transpiler, scope, init.get())
      if initNode.isNone:
        return none(CStmt)
      initNode
  return
    CStmt(
      kind: CskVarDecl,
      pos: variable.node.pos,
      comments: variable.node.comments,
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

proc transpileHFile(transpiler: Transpiler, globalSymbols: seq[Symbol]): Option[CStmt] =
  var moduleH = CStmt(
    kind: CskTranslationUnit,
    translationUnitNode: ast_c.TranslationUnitNode(
      includes: [
        ast_c.IncludeNode(isSystem: true, file: "<stdint.h>"),
        ast_c.IncludeNode(isSystem: true, file: "<stdbool.h>"),
      ].toHashSet()
    ),
  )
  for symbol in globalSymbols:
    if (symbol.kind == Function and not symbol.node.funDeclStmt.isPublic) or
        (symbol.kind == Variable and not symbol.node.varDeclStmt.isPublic):
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
        ast_c.IncludeNode(isSystem: isSystem, file: header)
      )
    if symbol.kind == Function:
      let function = transpileFunction(transpiler, symbol.scope, symbol, true)
      if function.isNone:
        return none(CStmt)
      moduleH.translationUnitNode.declarations.add(function.get())
    elif symbol.kind == Variable:
      let variable = transpileVariable(transpiler, nil, symbol, true)
      if variable.isNone:
        return none(CStmt)
      moduleH.translationUnitNode.declarations.add(variable.get())
    else:
      transpilerError(
        transpiler,
        "Invalid symbol kind for header: " & symbol.canonicalName,
        symbol.node.pos,
        "Remember to implement " & $symbol.kind,
      )
      return none(CStmt)
  result =
    CStmt(
      kind: CskIfNotDef,
      ifNotDefNode: ast_c.IfNotDefNode(
        name: transpiler.file.name.toUpperAscii() & "_H",
        body:
          @[
            CStmt(
              kind: CskDefine,
              defineNode: ast_c.DefineNode(
                name: transpiler.file.name.toUpperAscii() & "_H", value: none(CExpr)
              ),
            ), moduleH,
          ],
        elseBody: @[],
      ),
    ).some

proc generateMainFunction(transpiler: Transpiler): Option[CStmt] =
  ## Generate a C main function that calls the entry point
  if transpiler.entryPoint.isNone:
    return none(CStmt)
  let entry = transpiler.entryPoint.get()
  let entryName = entry.canonicalName
  let paramTypes = entry.paramTypes
  let returnTypeOpt = entry.returnType.toCType()
  if returnTypeOpt.isNone:
    transpilerError(
      transpiler, "Entry point return type cannot be mapped to C", entry.node.pos
    )
    return none(CStmt)
  let returnType = returnTypeOpt.get()
  # Only allow 0, 2, or 3 parameters
  if not (paramTypes.len == 0 or paramTypes.len == 2 or paramTypes.len == 3):
    transpilerError(
      transpiler, "Entry point must have 0, 2, or 3 parameters", entry.node.pos
    )
    return none(CStmt)
  # Only allow return type int or void
  let isInt =
    returnType.kind == CkPrimitive and
    (returnType.primitive == Int64T or returnType.primitive == Int32T)
  let isVoid = returnType.kind == CkPrimitive and returnType.primitive == VoidT
  if not (isInt or isVoid):
    transpilerError(transpiler, "Entry point must return Int or Void", entry.node.pos)
    return none(CStmt)
  # Build parameters for main
  var mainParams: seq[ast_c.ParameterNode] = @[]
  var callArgs: seq[CExpr] = @[]
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
      CExpr(kind: CekIdentifier, identifierNode: ast_c.IdentifierNode(name: "argc"))
    )
    callArgs.add(
      CExpr(kind: CekIdentifier, identifierNode: ast_c.IdentifierNode(name: "argv"))
    )
  elif paramTypes.len == 3:
    # int argc, char** argv, char** envp
    mainParams.add(ast_c.ParameterNode(name: "argc", paramType: int32T))
    mainParams.add(ast_c.ParameterNode(name: "argv", paramType: charPtrPtr))
    mainParams.add(ast_c.ParameterNode(name: "envp", paramType: charPtrPtr))
    callArgs.add(
      CExpr(kind: CekIdentifier, identifierNode: ast_c.IdentifierNode(name: "argc"))
    )
    callArgs.add(
      CExpr(kind: CekIdentifier, identifierNode: ast_c.IdentifierNode(name: "argv"))
    )
    callArgs.add(
      CExpr(kind: CekIdentifier, identifierNode: ast_c.IdentifierNode(name: "envp"))
    )
  # Build call to entry point
  let callEntry = CExpr(
    kind: CekFunctionCall,
    functionCallNode: ast_c.FunctionCallNode(
      callee: CExpr(
        kind: CekIdentifier, identifierNode: ast_c.IdentifierNode(name: entryName)
      ),
      arguments: callArgs,
    ),
  )
  var stmts: seq[CStmt] = @[]
  if isInt:
    stmts.add(
      CStmt(
        kind: CskReturnStmt,
        returnStmtNode: ast_c.ReturnStmtNode(expression: some(callEntry)),
      )
    )
  else:
    stmts.add(
      CStmt(kind: CskExprStmt, exprStmtNode: ast_c.ExprStmtNode(expression: callEntry))
    )
    stmts.add(
      CStmt(
        kind: CskReturnStmt,
        returnStmtNode: ast_c.ReturnStmtNode(
          expression:
            CExpr(kind: CekIntLiteral, intLiteralNode: ast_c.IntLiteralNode(value: 0)).some
        ),
      )
    )
  let mainDecl = CStmt(
    kind: CskFunctionDecl,
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
    CStmt(kind: CskBlockStmt, blockStmtNode: ast_c.BlockStmtNode(statements: stmts))
  return
    CStmt(
      kind: CskFunctionDef,
      functionDefNode: ast_c.FunctionDefNode(declaration: mainDecl, body: mainBody),
    ).some

proc transpileCFile(
    transpiler: Transpiler, globalSymbols: seq[Symbol], scope: Scope, node: Stmt
): Option[CStmt] =
  var moduleC = CStmt(
    kind: CskTranslationUnit,
    translationUnitNode: ast_c.TranslationUnitNode(
      includes:
        [ast_c.IncludeNode(isSystem: false, file: transpiler.file.name & ".h")].toHashSet()
    ),
  )
  # Add declarations and includes to the C file
  for symbol in globalSymbols:
    if (symbol.kind == Function and symbol.node.funDeclStmt.isPublic) or
        (symbol.kind == Variable and symbol.node.varDeclStmt.isPublic):
      continue
    let headerOpt = symbol.node.annotations["include", "from"]
    if headerOpt.isSome() and symbol.kind == Function and
        symbol.paramTypes[^1].kind == TkMeta and
        symbol.paramTypes[^1].metaKind == MkCVarArgs:
      # Only include if it's a varargs function, otherwise it's included in the C file
      let header: string = headerOpt.get()
      let isSystem = header.startsWith("<") and header.endsWith(">")
      moduleC.translationUnitNode.includes.incl(
        ast_c.IncludeNode(isSystem: isSystem, file: header)
      )
    if symbol.kind == Function:
      let function = transpileFunction(transpiler, symbol.scope, symbol, true)
      if function.isNone:
        return none(CStmt)
      moduleC.translationUnitNode.declarations.add(function.get())
    elif symbol.kind == Variable:
      let variable = transpileVariable(transpiler, scope, symbol, true)
      if variable.isNone:
        return none(CStmt)
      moduleC.translationUnitNode.declarations.add(variable.get())
    else:
      transpilerError(
        transpiler,
        "Invalid symbol kind for c file: " & symbol.canonicalName,
        symbol.node.pos,
        "Remember to implement " & $symbol.kind,
      )
      return none(CStmt)

  # Add implementations to the C file
  for symbol in globalSymbols:
    if symbol.kind == Function:
      let function = transpileFunction(transpiler, symbol.scope, symbol)
      if function.isNone:
        return none(CStmt)
      moduleC.translationUnitNode.declarations.add(function.get())
    elif symbol.kind == Variable:
      let variable = transpileVariable(transpiler, scope, symbol)
      if variable.isNone:
        return none(CStmt)
      moduleC.translationUnitNode.declarations.add(variable.get())
    else:
      transpilerError(
        transpiler,
        "Invalid symbol kind for c file: " & symbol.canonicalName,
        symbol.node.pos,
        "Remember to implement " & $symbol.kind,
      )
      return none(CStmt)

  # Add the main function if entry point is set
  let mainFunc = generateMainFunction(transpiler)
  if mainFunc.isSome:
    moduleC.translationUnitNode.declarations.add(mainFunc.get())

  result = moduleC.some

proc transpile*(file: FileInfo): tuple[hasError: bool, hFile: CStmt, cFile: CStmt] =
  let analyzer = newAnalyzer(file)
  let (hasError, scope, ast) = analyzer.analyze()
  let transpiler = Transpiler(file: file, hasError: hasError)
  if hasError:
    return (true, nil, nil)
  let globalSymbols = transpiler.getGlobalSymbols(scope)
  let hFile = transpileHFile(transpiler, globalSymbols)
  if hFile.isNone or transpiler.hasError:
    return (true, nil, nil)
  let cFile = transpileCFile(transpiler, globalSymbols, scope, ast)
  if cFile.isNone or transpiler.hasError:
    return (true, nil, nil)
  return (false, hFile.get(), cFile.get())
