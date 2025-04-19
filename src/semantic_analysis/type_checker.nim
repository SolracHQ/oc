import ../types/[file_info, position, scope, ast, types, token]
import ../reporter
import std/options
import std/tables
import type_inferencer

type TypeChecker* = ref object
  fileInfo*: FileInfo
  hasError*: bool

proc typeCheckError*(
    checker: TypeChecker, position: Position, msg: string, hint: string
) =
  ## Prints an error message for the type checker
  logError("TypeChecker", msg, position, hint)
  checker.hasError = true

proc newTypeChecker*(fileInfo: FileInfo): TypeChecker =
  ## Creates a new type checker for the given file
  result = TypeChecker()
  result.fileInfo = fileInfo
  result.hasError = false

proc areTypesCompatible(
    checker: TypeChecker, expected: Type, actual: Type, pos: Position, isVarArgs = false
): bool =
  ## Checks if the provided types are compatible
  ## If isVarArgs is true, allow any type when expected is the last parameter

  if isVarArgs:
    # For varargs, we accept any type
    return true

  # Direct match
  if expected == actual:
    return true

  # Special case: nil is compatible with pointer types
  if actual.kind == TkMeta and actual.metaKind == MkUnresolved and actual.name == "nil":
    # For now, just return true for any nil assignment (to be refined)
    return true

  if actual.kind == TkMeta and actual.metaKind == MkIntInfer and isIntFamily(expected):
    # Allow int inference for int family types
    return true
  if actual.kind == TkMeta and actual.metaKind == MkFloatInfer and
      isFloatFamily(expected):
    # Allow float inference for float family types
    return true
  if actual.kind == TkMeta and actual.metaKind == MkUIntInfer and isUIntFamily(expected):
    # Allow uint inference for uint family types
    return true

  # Types are not compatible
  checker.typeCheckError(
    pos, "Type mismatch", "expected '" & $expected & "', got '" & $actual & "'"
  )
  return false

proc analyzeTypeCheckingExpr(checker: TypeChecker, scope: Scope, expr: Expr) =
  ## Type check an expression node
  case expr.kind
  of EkAssignment:
    # Assignment is valid as an expression; check types
    let identifier = expr.assignmentExpr.identifier
    let value = expr.assignmentExpr.value
    let symbolOpt = scope.findSymbol(identifier, expr.pos, Variable)
    if symbolOpt.isNone:
      checker.typeCheckError(
        expr.pos,
        "Assignment to undeclared variable '" & identifier & "'",
        "use 'let' or 'var' to declare it",
      )
    else:
      let symbol = symbolOpt.get()
      let inferencer = newTypeInferencer(checker.fileInfo)
      let valueType = inferExpressionType(inferencer, scope, value)
      discard areTypesCompatible(checker, symbol.varType, valueType, expr.pos)
    # Type check both sides
    # (left is just an identifier, but for completeness)
    # If you support more complex lvalues, update here
    # analyzeTypeCheckingExpr(checker, scope, expr.assignmentExpr.left) # Not needed for identifier
    analyzeTypeCheckingExpr(checker, scope, value)
  of EkLogicalExpr:
    let inferencer = newTypeInferencer(checker.fileInfo)
    let leftType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.left)
    let rightType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.right)
    if leftType.kind != TkPrimitive or leftType.primitive != Bool:
      checker.typeCheckError(
        expr.binaryOpExpr.left.pos,
        "Logical operator requires boolean operands",
        "left operand is " & $leftType,
      )
    if rightType.kind != TkPrimitive or rightType.primitive != Bool:
      checker.typeCheckError(
        expr.binaryOpExpr.right.pos,
        "Logical operator requires boolean operands",
        "right operand is " & $rightType,
      )
    analyzeTypeCheckingExpr(checker, scope, expr.binaryOpExpr.left)
    analyzeTypeCheckingExpr(checker, scope, expr.binaryOpExpr.right)
  of EkEqualityExpr, EkComparisonExpr:
    let inferencer = newTypeInferencer(checker.fileInfo)
    let leftType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.left)
    let rightType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.right)
    if leftType.kind == TkPrimitive and rightType.kind == TkPrimitive:
      if leftType.primitive != rightType.primitive:
        checker.typeCheckError(
          expr.pos,
          "Type mismatch in comparison.",
          $leftType.primitive & " cannot be compared with " & $rightType.primitive,
        )
    analyzeTypeCheckingExpr(checker, scope, expr.binaryOpExpr.left)
    analyzeTypeCheckingExpr(checker, scope, expr.binaryOpExpr.right)
  of EkAdditiveExpr, EkMultiplicativeExpr:
    let inferencer = newTypeInferencer(checker.fileInfo)
    let leftType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.left)
    let rightType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.right)
    if (isIntFamily(leftType) and isIntFamily(rightType)) or
        (isFloatFamily(leftType) and isFloatFamily(rightType)) or
        (isUIntFamily(leftType) and isUIntFamily(rightType)):
      discard
    elif (isIntFamily(leftType) and isFloatFamily(rightType)) or
        (isFloatFamily(leftType) and isIntFamily(rightType)):
      checker.typeCheckError(
        expr.pos,
        "Type mismatch in arithmetic operation.",
        $leftType & " and " & $rightType & " (int and float types cannot be mixed)",
      )
    elif (isIntFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isIntFamily(rightType)):
      checker.typeCheckError(
        expr.pos,
        "Type mismatch in arithmetic operation.",
        $leftType & " and " & $rightType & " (int and uint types cannot be mixed)",
      )
    elif (isFloatFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isFloatFamily(rightType)):
      checker.typeCheckError(
        expr.pos,
        "Type mismatch in arithmetic operation.",
        $leftType & " and " & $rightType & " (float and uint types cannot be mixed)",
      )
    else:
      checker.typeCheckError(
        expr.pos,
        "Arithmetic operations require numeric types",
        "but got " & $leftType & " and " & $rightType,
      )
    analyzeTypeCheckingExpr(checker, scope, expr.binaryOpExpr.left)
    analyzeTypeCheckingExpr(checker, scope, expr.binaryOpExpr.right)
  of EkUnaryExpr:
    let inferencer = newTypeInferencer(checker.fileInfo)
    let operandType = inferExpressionType(inferencer, scope, expr.unaryOpExpr.operand)
    case expr.unaryOpExpr.operator
    of TkMinus:
      if not (
        isIntFamily(operandType) or isFloatFamily(operandType) or
        isUIntFamily(operandType)
      ):
        checker.typeCheckError(
          expr.pos, "Unary minus requires numeric operand", "but got " & $operandType
        )
    of TkBang:
      if operandType.kind != TkPrimitive or operandType.primitive != Bool:
        checker.typeCheckError(
          expr.pos,
          "Logical negation requires boolean operand",
          "but got " & $operandType,
        )
    else:
      checker.typeCheckError(
        expr.pos,
        "Unsupported unary operator: " & $expr.unaryOpExpr.operator,
        "This should not happen: " & $expr.unaryOpExpr.operator,
      )
    analyzeTypeCheckingExpr(checker, scope, expr.unaryOpExpr.operand)
  of EkAddressOfExpr:
    let inferencer = newTypeInferencer(checker.fileInfo)
    let operandType =
      inferExpressionType(inferencer, scope, expr.addressOfExpr.operand)
    if not operandType.hasAddress:
      checker.typeCheckError(
        expr.pos,
        "Address-of operator requires a value with an address",
        "but got " & $operandType,
      )
    analyzeTypeCheckingExpr(checker, scope, expr.addressOfExpr.operand)
  of EkDerefExpr:
    let inferencer = newTypeInferencer(checker.fileInfo)
    let operandType = inferExpressionType(inferencer, scope, expr.derefExpr.operand)
    if operandType.kind != TkPointer and operandType.kind != TkROPointer:
      checker.typeCheckError(
        expr.pos,
        "Dereference operator requires a pointer type",
        "but got " & $operandType,
      )
    analyzeTypeCheckingExpr(checker, scope, expr.derefExpr.operand)
  of EkMemberAccess:
    analyzeTypeCheckingExpr(checker, scope, expr.memberAccessExpr.obj)
  of EkFunctionCall:
    let callee = expr.functionCallExpr.callee
    let arguments = expr.functionCallExpr.arguments
    # Only handle identifier calls for now
    if callee.kind == EkIdentifier:
      let funcName = callee.identifierExpr.name
      let symbolOpt = scope.findSymbol(funcName, callee.pos, {Function, Variable})
      if symbolOpt.isNone:
        checker.typeCheckError(
          callee.pos,
          "Call to undefined function '" & funcName & "'",
          "Declare it with 'fun'",
        )
      elif symbolOpt.get().kind != Function:
        checker.typeCheckError(
          callee.pos,
          "Cannot call non-function '" & funcName & "'",
          "Function values will be supported in the future",
        )
      else:
        let funcSymbol = symbolOpt.get()
        let paramTypes = funcSymbol.paramTypes
        let hasVarArgs =
          paramTypes.len > 0 and paramTypes[^1].kind == TkMeta and
          paramTypes[^1].metaKind == MkCVarArgs
        if hasVarArgs:
          if arguments.len < paramTypes.len - 1:
            checker.typeCheckError(
              expr.pos,
              "Too few arguments to function '" & funcName & "'",
              "Expected at least " & $(paramTypes.len - 1) & " arguments but got " &
                $arguments.len,
            )
        if not hasVarArgs:
          if arguments.len < paramTypes.len:
            checker.typeCheckError(
              expr.pos,
              "Too few arguments to function '" & funcName & "'",
              "Expected " & $paramTypes.len & " arguments but got " & $arguments.len,
            )
          elif arguments.len > paramTypes.len:
            checker.typeCheckError(
              expr.pos,
              "Too many arguments to function '" & funcName & "'",
              "Expected " & $paramTypes.len & " arguments but got " & $arguments.len,
            )
        let inferencer = newTypeInferencer(checker.fileInfo)
        let regularParamCount =
          if hasVarArgs:
            paramTypes.len - 1
          else:
            paramTypes.len
        for i in 0 ..< min(regularParamCount, arguments.len):
          let argType = inferExpressionType(inferencer, scope, arguments[i])
          let paramType = paramTypes[i]
          discard areTypesCompatible(checker, paramType, argType, arguments[i].pos)
    analyzeTypeCheckingExpr(checker, scope, callee)
    for arg in arguments:
      analyzeTypeCheckingExpr(checker, scope, arg)
  of EkGroupExpr:
    analyzeTypeCheckingExpr(checker, scope, expr.groupExpr.expression)
  # Leaf nodes
  of EkIdentifier, EkIntLiteral, EkUIntLiteral, EkFloatLiteral, EkStringLiteral,
      EkCStringLiteral, EkCharLiteral, EkBoolLiteral, EkNilLiteral, EkType:
    discard

proc analyzeTypeChecking*(checker: TypeChecker, scope: Scope, stmt: Stmt) =
  ## Check that all types match correctly in statements and their contained expressions
  case stmt.kind
  of SkModule:
    for s in stmt.moduleStmt.statements:
      analyzeTypeChecking(checker, scope, s)
  of SkVarDecl:
    let varDecl = stmt.varDeclStmt
    if varDecl.initializer.isSome:
      let initExpr = varDecl.initializer.get()
      let inferencer = newTypeInferencer(checker.fileInfo)
      let initType = inferExpressionType(inferencer, scope, initExpr)
      discard areTypesCompatible(checker, varDecl.typeAnnotation, initType, stmt.pos)
      analyzeTypeCheckingExpr(checker, scope, initExpr)
  of SkFunDecl:
    let funDecl = stmt.funDeclStmt
    if funDecl.body.isSome:
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeTypeChecking(checker, functionScope, funDecl.body.get())
  of SkBlockStmt:
    let blockScope = scope.children[stmt.blockStmt.blockId]
    for s in stmt.blockStmt.statements:
      analyzeTypeChecking(checker, blockScope, s)
  of SkExprStmt:
    analyzeTypeCheckingExpr(checker, scope, stmt.exprStmt.expression)
  of SkReturnStmt:
    if stmt.returnStmt.expression.isSome:
      let returnExpr = stmt.returnStmt.expression.get()
      var currentScope = scope
      var funcSymbol: Symbol = nil
      while currentScope != nil:
        if currentScope.kind == FunctionScope:
          let parentScope = currentScope.parent
          if parentScope != nil:
            let funcName = currentScope.name
            let symbolOpt = parentScope.findSymbol(funcName, stmt.pos, Function)
            if symbolOpt.isSome:
              funcSymbol = symbolOpt.get()
          break
        currentScope = currentScope.parent
      if funcSymbol != nil:
        let inferencer = newTypeInferencer(checker.fileInfo)
        let exprType = inferExpressionType(inferencer, scope, returnExpr)
        discard areTypesCompatible(checker, funcSymbol.returnType, exprType, stmt.pos)
      else:
        checker.typeCheckError(
          stmt.pos,
          "Return statement outside of function",
          "Return statements must be inside a function",
        )
      analyzeTypeCheckingExpr(checker, scope, returnExpr)
  of SkIfStmt:
    for i, branch in stmt.ifStmt.branches:
      let branchScope = scope.children[branch.scopeId]
      let inferencer = newTypeInferencer(checker.fileInfo)
      let condType = inferExpressionType(inferencer, scope, branch.condition)
      if condType.kind != TkPrimitive or condType.primitive != Bool:
        checker.typeCheckError(
          branch.condition.pos,
          (if i == 0: "if" else: "elif") & " condition must be a boolean expression",
          "but got " & $condType,
        )
      analyzeTypeCheckingExpr(checker, branchScope, branch.condition)
      analyzeTypeChecking(checker, branchScope, branch.body)
    if stmt.ifStmt.elseBranch.isSome:
      let elseBranch = stmt.ifStmt.elseBranch.get
      let elseScope = scope.children[elseBranch.scopeId]
      analyzeTypeChecking(checker, elseScope, elseBranch.body)
  of SkNop:
    discard
