import ../types/[scope, ast, types]
import std/options
import std/tables

proc expandStructLiteral(scope: Scope, expr: var Expr) =
  if expr.kind != EkStructLiteral:
    return
  let structTypeName = expr.structLiteralExpr.typeName
  let structSymOpt = scope.findSymbol(structTypeName, expr.pos, Type)
  if structSymOpt.isNone:
    return
  let structSym = structSymOpt.get
  if structSym.kind != Type:
    return
  if structSym.typeRepr.kind != TkStruct:
    return
  let structType = structSym.declStmt.structDeclStmt
  var present = initTable[string, bool]()
  for m in expr.structLiteralExpr.members:
    present[m.name] = true
  for name, member in structType.members:
    if not present.hasKey(name) and member.defaultValue.isSome:
      expr.structLiteralExpr.members.add StructLiteralMember(
        name: name, namePos: expr.pos, value: member.defaultValue.get
      )

proc expandFunctionCall(scope: Scope, expr: var Expr) =
  if expr.kind != EkFunctionCall:
    return
  let callee = expr.functionCallExpr.callee
  if callee.kind != EkIdentifier:
    return
  let funName = callee.identifierExpr.name
  let funSymOpt = scope.findSymbol(funName, expr.pos, Function)
  if funSymOpt.isNone:
    return
  let funSym = funSymOpt.get
  if funSym.kind != Function:
    return
  let funDecl = funSym.declStmt.funDeclStmt
  let params = funDecl.parameters
  var args = expr.functionCallExpr.arguments
  for i in args.len ..< params.len - 1:
    if params[i].defaultValue.isSome:
      args.add params[i].defaultValue.get
  expr.functionCallExpr.arguments = args

proc expandLiteralsInExpr(scope: Scope, expr: var Expr) =
  case expr.kind
  of EkStructLiteral:
    expandStructLiteral(scope, expr)
    for m in expr.structLiteralExpr.members.mitems:
      expandLiteralsInExpr(scope, m.value)
  of EkFunctionCall:
    expandFunctionCall(scope, expr)
    for a in expr.functionCallExpr.arguments.mitems:
      expandLiteralsInExpr(scope, a)
  of EkAssignment:
    expandLiteralsInExpr(scope, expr.assignmentExpr.left)
    expandLiteralsInExpr(scope, expr.assignmentExpr.value)
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr,
      EkMultiplicativeExpr:
    expandLiteralsInExpr(scope, expr.binaryOpExpr.left)
    expandLiteralsInExpr(scope, expr.binaryOpExpr.right)
  of EkUnaryExpr:
    expandLiteralsInExpr(scope, expr.unaryOpExpr.operand)
  of EkMemberAccess:
    expandLiteralsInExpr(scope, expr.memberAccessExpr.obj)
  of EkGroupExpr:
    expandLiteralsInExpr(scope, expr.groupExpr.expression)
  of EkAddressOfExpr:
    expandLiteralsInExpr(scope, expr.addressOfExpr.operand)
  of EkDerefExpr:
    expandLiteralsInExpr(scope, expr.derefExpr.operand)
  else:
    discard

proc expandLiteralsInStmt(scope: Scope, stmt: var Stmt) =
  case stmt.kind
  of SkModule:
    for s in stmt.moduleStmt.statements.mitems:
      expandLiteralsInStmt(scope, s)
  of SkVarDecl:
    if stmt.varDeclStmt.initializer.isSome:
      expandLiteralsInExpr(scope, stmt.varDeclStmt.initializer.get)
  of SkFunDecl:
    if stmt.funDeclStmt.body.isSome:
      expandLiteralsInStmt(scope, stmt.funDeclStmt.body.get)
  of SkTypeDecl, SkStructDecl:
    discard
  of SkBlockStmt:
    for s in stmt.blockStmt.statements.mitems:
      expandLiteralsInStmt(scope, s)
  of SkExprStmt:
    expandLiteralsInExpr(scope, stmt.exprStmt.expression)
  of SkReturnStmt:
    if stmt.returnStmt.expression.isSome:
      expandLiteralsInExpr(scope, stmt.returnStmt.expression.get)
  of SkIfStmt:
    for b in stmt.ifStmt.branches.mitems:
      expandLiteralsInExpr(scope, b.condition)
      expandLiteralsInStmt(scope, b.body)
    if stmt.ifStmt.elseBranch.isSome:
      expandLiteralsInStmt(scope, stmt.ifStmt.elseBranch.get.body)
  of SkWhileStmt:
    expandLiteralsInExpr(scope, stmt.whileStmt.condition)
    expandLiteralsInStmt(scope, stmt.whileStmt.body)
  of SkNop:
    discard

proc expandLiterals*(scope: Scope, module: var Stmt) =
  expandLiteralsInStmt(scope, module)
