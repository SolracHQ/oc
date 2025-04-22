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
    transpiler: Transpiler, pos: Position, error: string, hint: string = ""
) =
  ## Log an error during transpilation
  logError("Transpiler", pos, error, hint)
  transpiler.hasError = true

proc transpileVariable(
  transpiler: Transpiler, scope: Scope, variable: Symbol, declarationOnly: bool = false
): Option[CStmt]

proc makePointerType(base: CType): CType =
  result = CType(kind: CkPointer, ctype: base)

proc getGlobalSymbols(transpiler: Transpiler, scope: Scope): seq[Symbol] =
  ## Get all global symbols from the scope
  for (name, symbol) in scope.symbols.pairs():
    if symbol.kind == Variable and scope.kind == ModuleScope:
      result.add(symbol)
    elif symbol.kind == Function:
      result.add(symbol)
      if symbol.declStmt.annotations.hasAnnotation("entrypoint"):
        if transpiler.entryPoint.isNone:
          transpiler.entryPoint = some(symbol)
        else:
          transpilerError(
            transpiler,
            symbol.declStmt.pos,
            "Multiple entry points found: " & symbol.canonicalName,
          )
    elif symbol.kind == Type:
      result.add(symbol)
  for child in scope.children.values():
    result.add(getGlobalSymbols(transpiler, child))
  result.sort((s1, s2) => s1.kind.int.cmp s2.kind.int)

proc transpileStructDecl(
    transpiler: Transpiler, symbol: Symbol, stmt: Stmt
): Option[CStmt] =
  # Transpile a struct declaration to a C struct definition
  let structName = symbol.canonicalName
  var members: seq[ast_c.ParameterNode] = @[]
  for mname, m in stmt.structDeclStmt.members:
    let memberType = m.memberType.toCType()
    if memberType.isNone:
      transpilerError(
        transpiler,
        m.namePos,
        "Invalid struct member type for: " & m.name,
        "Metatypes cannot be mapped to C types",
      )
      return none(CStmt)
    members.add(ast_c.ParameterNode(name: m.name, paramType: memberType.get()))
  result = some(
    CStmt(
      kind: CskStructDef,
      pos: stmt.pos,
      comments: stmt.comments,
      structDefNode: ast_c.StructDefNode(name: structName, members: members),
    )
  )

proc transpileTypeDecl(
    transpiler: Transpiler, symbol: Symbol, stmt: Stmt
): Option[CStmt] =
  # Transpile a type declaration to a C typedef
  let typeName = symbol.canonicalName
  let baseType = stmt.typeDeclStmt.typeAnnotation.toCType()
  if baseType.isNone:
    transpilerError(
      transpiler,
      stmt.pos,
      "Invalid typedef base type for: " & typeName,
      "Metatypes cannot be mapped to C types",
    )
    return none(CStmt)
  result = some(
    CStmt(
      kind: CskTypedef,
      pos: stmt.pos,
      comments: stmt.comments,
      typedefNode: ast_c.TypedefNode(name: typeName, baseType: baseType.get()),
    )
  )

proc transpileExpr(transpiler: Transpiler, scope: Scope, expr: Expr): Option[CExpr]

proc transpileStructLiteral(
    transpiler: Transpiler, scope: Scope, expr: Expr
): Option[CExpr] =
  # Transpile a struct literal expression to a C struct literal
  let typeName = expr.structLiteralExpr.typeName
  let symbol = scope.findSymbol(typeName, expr.pos, Type)
  if symbol.isNone:
    transpilerError(
      transpiler,
      expr.pos,
      "Cannot find struct type: " & typeName,
      "Struct '" & typeName & "' not found",
    )
    return none(CExpr)
  let canonicalName = symbol.get().canonicalName
  var members: seq[ast_c.StructLiteralMemberNode] = @[]
  for m in expr.structLiteralExpr.members:
    let valueC = transpileExpr(transpiler, scope, m.value)
    if valueC.isNone:
      return none(CExpr)
    members.add(ast_c.StructLiteralMemberNode(name: m.name, value: valueC.get()))
  result = some(
    CExpr(
      kind: CekStructLiteral,
      pos: expr.pos,
      structLiteralNode:
        ast_c.StructLiteralNode(typeName: canonicalName, members: members),
    )
  )

proc transpileExpr(transpiler: Transpiler, scope: Scope, expr: Expr): Option[CExpr] =
  case expr.kind
  of EkAssignment:
    let lhs = transpileExpr(transpiler, scope, expr.assignmentExpr.left)
    let rhsC = transpileExpr(transpiler, scope, expr.assignmentExpr.value)
    if rhsC.isNone or lhs.isNone:
      return none(CExpr)
    result = some(
      CExpr(
        kind: CekAssignment,
        pos: expr.pos,
        assignmentNode:
          ast_c.AssignmentNode(lhs: lhs.get(), rhs: rhsC.get(), operator: TkEqual),
      )
    )
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr,
      EkMultiplicativeExpr:
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
        unaryExprNode: ast_c.UnaryExprNode(
          operator: expr.unaryOpExpr.operator, operand: operandC.get()
        ),
      )
    )
  of EkMemberAccess:
    let objC = transpileExpr(transpiler, scope, expr.memberAccessExpr.obj)
    if objC.isNone:
      return none(CExpr)
    # Determine if the object is a pointer type
    let objType = expr.memberAccessExpr.obj.exprType
    if objType.kind == TkPointer:
      # Use arrow operator
      result = some(
        CExpr(
          kind: CekArrowMemberAccess,
          pos: expr.pos,
          arrowMemberAccessNode: ast_c.ArrowMemberAccessNode(
            expr: objC.get(), member: expr.memberAccessExpr.member
          ),
        )
      )
    else:
      # Use dot operator
      result = some(
        CExpr(
          kind: CekMemberAccess,
          pos: expr.pos,
          memberAccessNode: ast_c.MemberAccessNode(
            expr: objC.get(), member: expr.memberAccessExpr.member
          ),
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
        transpiler, expr.pos, "Unknown identifier: " & expr.identifierExpr.name
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
  of EkStructLiteral:
    result = transpileStructLiteral(transpiler, scope, expr)

proc transpileStmt(transpiler: Transpiler, scope: Scope, stmt: Stmt): Option[CStmt] =
  case stmt.kind
  of SkModule:
    discard # handled at file level
  of SkVarDecl:
    let variableSymbol =
      scope.findSymbol(stmt.varDeclStmt.identifier, stmt.pos, Variable)
    if variableSymbol.isNone:
      transpilerError(transpiler, stmt.pos, "Cannot find variable declaration in scope")
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
  of SkWhileStmt:
    let whileStmt = stmt.whileStmt
    let whileScope = scope.children[whileStmt.scopeId]
    let condC = transpileExpr(transpiler, whileScope, whileStmt.condition)
    let bodyC = transpileStmt(transpiler, whileScope, whileStmt.body)
    if condC.isNone or bodyC.isNone:
      return none(CStmt)
    result = some(
      CStmt(
        kind: CskWhileStmt,
        pos: stmt.pos,
        comments: stmt.comments,
        whileStmtNode: ast_c.WhileStmtNode(condition: condC.get(), body: bodyC.get()),
      )
    )
  of SkNop:
    return none(CStmt)
  of SkStructDecl:
    let symbol = scope.findSymbol(stmt.structDeclStmt.identifier, stmt.pos, Type)
    if symbol.isNone:
      transpilerError(
        transpiler,
        stmt.pos,
        "Cannot find struct declaration in scope",
        "Struct '" & stmt.structDeclStmt.identifier & "' not found",
      )
      return none(CStmt)
    result = transpileStructDecl(transpiler, symbol.get(), stmt)
  of SkTypeDecl:
    let symbol = scope.findSymbol(stmt.typeDeclStmt.identifier, stmt.pos, Type)
    if symbol.isNone:
      transpilerError(
        transpiler,
        stmt.pos,
        "Cannot find type declaration in scope",
        "Type '" & stmt.typeDeclStmt.identifier & "' not found",
      )
      return none(CStmt)
    result = transpileTypeDecl(transpiler, symbol.get(), stmt)

proc transpileFunction(
    transpiler: Transpiler,
    scope: Scope,
    function: Symbol,
    declarationOnly: bool = false,
): Option[CStmt] =
  let funDecl = function.declStmt.funDeclStmt
  if function.declStmt.annotations["include"].isSome() and
      funDecl.parameters.anyIt(
        it.paramType.kind == TkMeta and it.paramType.metaKind == MkCVarArgs
      ):
    return
      CStmt(
        kind: CskDefine,
        pos: function.declStmt.pos,
        comments: function.declStmt.comments,
        defineNode: ast_c.DefineNode(
          name: function.canonicalName,
          value:
            CExpr(
              kind: CekIdentifier,
              pos: function.declStmt.pos,
              identifierNode: ast_c.IdentifierNode(
                name: function.declStmt.annotations["include", "name"].get()
              ),
            ).some,
        ),
      ).some
  let name = function.canonicalName
  let returnType = funDecl.returnType.toCType()
  if returnType.isNone:
    transpilerError(
      transpiler,
      funDecl.identifierPos,
      "Invalid return type for function: " & name,
      "Metatypes cannot be mapped to C types",
    )
    return none(CStmt)
  let returnTypeC: CType = returnType.get()
  let isStatic = function.declStmt.annotations["static"].isSome()
  let isExtern = function.declStmt.annotations["extern"].isSome()
  let isInline = function.declStmt.annotations["inline"].isSome()
  var parameters: seq[ast_c.ParameterNode] = @[]
  for param in funDecl.parameters:
    let paramType = param.paramType.toCType()
    if paramType.isNone:
      transpilerError(
        transpiler,
        param.namePos,
        "Invalid parameter type for function: " & name,
        "Metatypes cannot be mapped to C types" & $param.paramType,
      )
      return none(CStmt)
    let paramTypeC: CType = paramType.get()
    parameters.add(ast_c.ParameterNode(name: param.name, paramType: paramTypeC))
  if declarationOnly or isExtern:
    return
      CStmt(
        kind: CskFunctionDecl,
        pos: funDecl.identifierPos,
        comments: function.declStmt.comments,
        functionDeclNode: ast_c.FunctionDeclNode(
          name: name,
          returnType: returnTypeC,
          parameters: parameters,
          isStatic: isStatic,
          isExtern: isExtern,
          isInline: isInline,
        ),
      ).some
  let body = funDecl.body
  if body.isNone:
    transpilerError(
      transpiler,
      funDecl.identifierPos,
      "Function body is None for function: " & name,
      "Function must have a body",
    )
    return none(CStmt)
  let bodyNode =
    transpileStmt(transpiler, scope.children[funDecl.identifier], body.get())
  if bodyNode.isNone:
    transpilerError(
      transpiler,
      funDecl.identifierPos,
      "Invalid function body for function: " & name,
      "Function body must be a block statement",
    )
    return none(CStmt)
  let bodyC: CStmt = bodyNode.get()
  return
    CStmt(
      kind: CskFunctionDef,
      pos: funDecl.identifierPos,
      comments: function.declStmt.comments,
      functionDefNode: ast_c.FunctionDefNode(
        declaration: CStmt(
          kind: CskFunctionDecl,
          pos: funDecl.identifierPos,
          comments: function.declStmt.comments,
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
  let varType = variable.declStmt.varDeclStmt.typeAnnotation.toCType()
  if varType.isNone:
    transpilerError(
      transpiler,
      variable.declStmt.pos,
      "Invalid variable type '" & $variable.declStmt.varDeclStmt.typeAnnotation &
        "' for variable: " & name,
      "Metatypes cannot be mapped to C types",
    )
    return none(CStmt)
  let varTypeC: CType = varType.get()
  let isStatic = variable.declStmt.annotations["static"].isSome()
  let isExtern = variable.declStmt.annotations["extern"].isSome()
  let isVolatile = variable.declStmt.annotations["volatile"].isSome()
  let isConst = variable.declStmt.varDeclStmt.isReadOnly
  let initializer =
    if declarationOnly or isExtern or variable.declStmt.varDeclStmt.initializer.isNone:
      none(CExpr)
    else:
      let init = variable.declStmt.varDeclStmt.initializer
      let initNode = transpileExpr(transpiler, scope, init.get())
      if initNode.isNone:
        return none(CStmt)
      initNode
  return
    CStmt(
      kind: CskVarDecl,
      pos: variable.declStmt.pos,
      comments: variable.declStmt.comments,
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
  # Add struct/type definitions before declarations
  for symbol in globalSymbols:
    if symbol.kind == Type and symbol.isPublic:
      let typeNode = symbol.declStmt
      if typeNode.kind == SkStructDecl:
        let structDef = transpileStructDecl(transpiler, symbol, typeNode)
        if structDef.isNone:
          return none(CStmt)
        moduleH.translationUnitNode.declarations.add(structDef.get())
      elif typeNode.kind == SkTypeDecl:
        let typedefDef = transpileTypeDecl(transpiler, symbol, typeNode)
        if typedefDef.isNone:
          return none(CStmt)
        moduleH.translationUnitNode.declarations.add(typedefDef.get())
  # Add declarations
  for symbol in globalSymbols:
    if (symbol.kind == Function and not symbol.declStmt.funDeclStmt.isPublic) or
        (symbol.kind == Variable and not symbol.declStmt.varDeclStmt.isPublic):
      # Only include public functions and variables
      continue
    let headerOpt = symbol.declStmt.annotations["include", "from"]
    if headerOpt.isSome() and symbol.kind == Function and
        symbol.declStmt.funDeclStmt.parameters[^1].paramType.kind == TkMeta and
        symbol.declStmt.funDeclStmt.parameters[^1].paramType.metaKind == MkCVarArgs:
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
  let funDecl = entry.declStmt.funDeclStmt
  let paramTypes = funDecl.parameters.mapIt(it.paramType)
  let returnTypeOpt = funDecl.returnType.toCType()
  if returnTypeOpt.isNone:
    transpilerError(
      transpiler, entry.declStmt.pos, "Entry point return type cannot be mapped to C"
    )
    return none(CStmt)
  let returnType = returnTypeOpt.get()
  # Only allow 0, 2, or 3 parameters
  if not (paramTypes.len == 0 or paramTypes.len == 2 or paramTypes.len == 3):
    transpilerError(
      transpiler, entry.declStmt.pos, "Entry point must have 0, 2, or 3 parameters"
    )
    return none(CStmt)
  # Only allow return type int or void
  let isInt =
    returnType.kind == CkPrimitive and
    (returnType.primitive == Int64T or returnType.primitive == Int32T)
  let isVoid = returnType.kind == CkPrimitive and returnType.primitive == VoidT
  if not (isInt or isVoid):
    transpilerError(
      transpiler, entry.declStmt.pos, "Entry point must return Int or Void"
    )
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
      includes: [ast_c.IncludeNode(isSystem: false, file: transpiler.file.name & ".h")].toHashSet()
    ),
  )
  # Add struct/type definitions before declarations
  for symbol in globalSymbols:
    if symbol.kind == Type and not symbol.isPublic:
      let typeNode = symbol.declStmt
      if typeNode.kind == SkStructDecl:
        let structDef = transpileStructDecl(transpiler, symbol, typeNode)
        if structDef.isNone:
          return none(CStmt)
        moduleC.translationUnitNode.declarations.add(structDef.get())
      elif typeNode.kind == SkTypeDecl:
        let typedefDef = transpileTypeDecl(transpiler, symbol, typeNode)
        if typedefDef.isNone:
          return none(CStmt)
        moduleC.translationUnitNode.declarations.add(typedefDef.get())
  # Add declarations and includes to the C file
  for symbol in globalSymbols:
    if (symbol.kind == Function and symbol.declStmt.funDeclStmt.isPublic) or
        (symbol.kind == Variable and symbol.declStmt.varDeclStmt.isPublic):
      continue
    let headerOpt = symbol.declStmt.annotations["include", "from"]
    if headerOpt.isSome() and symbol.kind == Function and
        symbol.declStmt.funDeclStmt.parameters[^1].paramType.kind == TkMeta and
        symbol.declStmt.funDeclStmt.parameters[^1].paramType.metaKind == MkCVarArgs:
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
