import ../types/[position, scope, ast, types]
import ../reporter
import std/options
import std/tables

type ReachabilityChecker* = ref object
  hasError*: bool

proc reachabilityError*(
    checker: ReachabilityChecker, position: Position, msg: string, hint: string = ""
) =
  ## Prints an error message for the reachability checker  
  logError("ReachabilityChecker", position, msg, hint)
  checker.hasError = true

proc newReachabilityChecker*(): ReachabilityChecker =
  ## Creates a new reachability checker for the given file
  result = ReachabilityChecker()
  result.hasError = false

proc analyzeReachability(checker: ReachabilityChecker, scope: Scope, expr: Expr) =
  case expr.kind
  of EkAssignment:
    # Analyze the right side (value) first
    analyzeReachability(checker, scope, expr.assignmentExpr.value)

    let left = expr.assignmentExpr.left
    analyzeReachability(checker, scope, left)

    # in case of a variable assignment, check if the variable is declared
    if left.kind == EkIdentifier:
      let identifier = left.identifierExpr.name
      let symbolOpt = scope.findSymbol(identifier, expr.pos, Variable)

      if symbolOpt.isNone:
        checker.reachabilityError(
          expr.pos, "Cannot assign to undeclared variable '" & identifier & "'"
        )
      else:
        let symbol = symbolOpt.get()

        if symbol.isReadOnly and symbol.isInitialized:
          # Error if trying to assign to readonly variable that's already initialized
          checker.reachabilityError(
            expr.pos, "Cannot reassign to readonly variable '" & identifier & "'"
          )
        else:
          # Mark the variable as initialized after successful assignment
          symbol.isInitialized = true
  of EkIdentifier:
    # Verify variable is initialized before use
    let identifier = expr.identifierExpr.name
    let symbolOpt = scope.findSymbol(identifier, expr.pos, AnySymbol)

    if symbolOpt.isNone:
      checker.reachabilityError(expr.pos, "Undeclared identifier '" & identifier & "'")
    elif symbolOpt.get().kind == Variable and not symbolOpt.get().isInitialized:
      checker.reachabilityError(
        expr.pos, "Variable '" & identifier & "' used before initialization"
      )
  of EkFunctionCall:
    # Check the callee and all arguments
    analyzeReachability(checker, scope, expr.functionCallExpr.callee)
    for arg in expr.functionCallExpr.arguments:
      analyzeReachability(checker, scope, arg)
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr,
      EkMultiplicativeExpr:
    analyzeReachability(checker, scope, expr.binaryOpExpr.left)
    analyzeReachability(checker, scope, expr.binaryOpExpr.right)
  of EkUnaryExpr:
    analyzeReachability(checker, scope, expr.unaryOpExpr.operand)
  of EkMemberAccess:
    analyzeReachability(checker, scope, expr.memberAccessExpr.obj)
  of EkGroupExpr:
    analyzeReachability(checker, scope, expr.groupExpr.expression)
  of EkAddressOfExpr:
    analyzeReachability(checker, scope, expr.addressOfExpr.operand)
  of EkDerefExpr:
    analyzeReachability(checker, scope, expr.derefExpr.operand)
  of EkStructLiteral:
    # Check reachability of each member value in struct literal
    for member in expr.structLiteralExpr.members:
      analyzeReachability(checker, scope, member.value)
  of EkArrayAccess:
    # Check reachability of array and index expressions
    analyzeReachability(checker, scope, expr.arrayAccessExpr.arrayExpr)
    analyzeReachability(checker, scope, expr.arrayAccessExpr.indexExpr)
  of EkArrayLiteral:
    # Check reachability of each element in array literal
    for element in expr.arrayLiteralExpr.elements:
      analyzeReachability(checker, scope, element)
  # Literals and type nodes don't involve variables
  of EkIntLiteral, EkUIntLiteral, EkFloatLiteral, EkStringLiteral, EkCStringLiteral,
      EkCharLiteral, EkBoolLiteral, EkNilLiteral:
    discard

proc checkTypeReachability(
    checker: ReachabilityChecker, scope: Scope, typ: Type, pos: Position
) =
  # Checks if a type node is unresolved and if so, verifies it exists in scope
  if typ.kind == TkMeta and typ.metaKind == MkNamedType:
    let typeName = typ.name
    let typeSym = scope.findSymbol(typeName, pos, Type)
    if typeSym.isNone:
      checker.reachabilityError(pos, "Type '" & typeName & "' does not exist")

proc analyzeReachability*(checker: ReachabilityChecker, scope: Scope, stmt: Stmt) =
  case stmt.kind
  of SkModule:
    for s in stmt.moduleStmt.statements:
      analyzeReachability(checker, scope, s)
  of SkVarDecl:
    checkTypeReachability(checker, scope, stmt.varDeclStmt.typeAnnotation, stmt.pos)
    if stmt.varDeclStmt.initializer.isSome:
      analyzeReachability(checker, scope, stmt.varDeclStmt.initializer.get)
  of SkFunDecl:
    let funDecl = stmt.funDeclStmt
    for param in funDecl.parameters:
      checkTypeReachability(checker, scope, param.paramType, param.paramTypePos)
    checkTypeReachability(checker, scope, funDecl.returnType, funDecl.returnTypePos)
    if funDecl.body.isSome:
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeReachability(checker, functionScope, funDecl.body.get)
  of SkTypeDecl:
    checkTypeReachability(checker, scope, stmt.typeDeclStmt.typeAnnotation, stmt.pos)
  of SkStructDecl:
    for _, member in stmt.structDeclStmt.members:
      checkTypeReachability(checker, scope, member.memberType, member.memberTypePos)
      if member.defaultValue.isSome:
        analyzeReachability(checker, scope, member.defaultValue.get)
  of SkBlockStmt:
    let blockScope = scope.children[stmt.blockStmt.blockId]
    for s in stmt.blockStmt.statements:
      analyzeReachability(checker, blockScope, s)
  of SkIfStmt:
    for branch in stmt.ifStmt.branches:
      let branchScope = scope.children[branch.scopeId]
      analyzeReachability(checker, scope, branch.condition)
      analyzeReachability(checker, branchScope, branch.body)
    if stmt.ifStmt.elseBranch.isSome:
      let elseBranch = stmt.ifStmt.elseBranch.get
      let elseScope = scope.children[elseBranch.scopeId]
      analyzeReachability(checker, elseScope, elseBranch.body)
  of SkWhileStmt:
    let whileStmt = stmt.whileStmt
    let whileScope = scope.children[whileStmt.scopeId]
    analyzeReachability(checker, scope, whileStmt.condition)
    analyzeReachability(checker, whileScope, whileStmt.body)
  of SkExprStmt:
    analyzeReachability(checker, scope, stmt.exprStmt.expression)
  of SkReturnStmt:
    if stmt.returnStmt.expression.isSome:
      analyzeReachability(checker, scope, stmt.returnStmt.expression.get)
  of SkNop:
    discard
