import ../types/[file_info, position, scope, ast]
import ../reporter
import std/options
import std/tables

type ReachabilityChecker* = ref object
  fileInfo*: FileInfo
  hasError*: bool

proc reachabilityError*(checker: ReachabilityChecker, position: Position, msg: string) =
  ## Prints an error message for the reachability checker  
  logError("ReachabilityChecker", msg, position)
  checker.hasError = true

proc newReachabilityChecker*(fileInfo: FileInfo): ReachabilityChecker =
  ## Creates a new reachability checker for the given file
  result = ReachabilityChecker()
  result.fileInfo = fileInfo
  result.hasError = false

proc analyzeReachability(checker: ReachabilityChecker, scope: Scope, expr: Expr) =
  case expr.kind
  of EkAssignment:
    # Analyze the right side (value) first
    analyzeReachability(checker, scope, expr.assignmentExpr.value)

    # Check if variable exists and can be assigned
    let identifier = expr.assignmentExpr.identifier
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
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr, EkMultiplicativeExpr:
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
  # Literals and type nodes don't involve variables
  of EkIntLiteral, EkUIntLiteral, EkFloatLiteral, EkStringLiteral, EkCStringLiteral,
      EkCharLiteral, EkBoolLiteral, EkNilLiteral, EkType:
    discard

proc analyzeReachability*(checker: ReachabilityChecker, scope: Scope, stmt: Stmt) =
  case stmt.kind
  of SkModule:
    for s in stmt.moduleStmt.statements:
      analyzeReachability(checker, scope, s)
  of SkVarDecl:
    if stmt.varDeclStmt.initializer.isSome:
      analyzeReachability(checker, scope, stmt.varDeclStmt.initializer.get)
  of SkFunDecl:
    let funDecl = stmt.funDeclStmt
    if funDecl.body.isSome:
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeReachability(checker, functionScope, funDecl.body.get)
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
  of SkExprStmt:
    analyzeReachability(checker, scope, stmt.exprStmt.expression)
  of SkReturnStmt:
    if stmt.returnStmt.expression.isSome:
      analyzeReachability(checker, scope, stmt.returnStmt.expression.get)
  of SkNop:
    discard
