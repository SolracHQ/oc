import ../types/[position, scope, ast, types, token]
import ../reporter
import std/options
import std/tables
import type_inferencer

type TypeChecker* = ref object
  hasError*: bool

proc typeCheckError*(
    checker: TypeChecker, position: Position, msg: string, hint: string
) =
  ## Prints an error message for the type checker
  logError("TypeChecker", position, msg, hint)
  checker.hasError = true

proc newTypeChecker*(): TypeChecker =
  ## Creates a new type checker for the given file
  result = TypeChecker()
  result.hasError = false

proc areTypesCompatible(
    checker: TypeChecker,
    scope: Scope,
    expected: Type,
    actual: Type,
    pos: Position,
    isVarArgs = false,
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
  if actual.kind == TkMeta and actual.metaKind == MkAnyPointer:
    # For now, just return true for any nil assignment (to be refined)
    return true

  if actual.kind == TkMeta and actual.metaKind == MkAnyInt and isIntFamily(expected):
    # Allow int inference for int family types
    return true
  if actual.kind == TkMeta and actual.metaKind == MkAnyFloat and isFloatFamily(expected):
    # Allow float inference for float family types
    return true
  if actual.kind == TkMeta and actual.metaKind == MkAnyUint and isUIntFamily(expected):
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
    let left = expr.assignmentExpr.left
    let value = expr.assignmentExpr.value

    # Check left side is assignable (has address or is pointer, not ro pointer)
    let leftType = left.exprType
    var canAssign = leftType.hasAddress

    if not canAssign:
      checker.typeCheckError(
        expr.pos, "Left side of assignment is not assignable",
        "Only variables or pointer dereferences can be assigned to",
      )
    else:
      # If left is identifier, check it exists (optional, like reachability)
      if left.kind == EkIdentifier:
        let identifier = left.identifierExpr.name
        let symbolOpt = scope.findSymbol(identifier, expr.pos, Variable)
        if symbolOpt.isNone:
          checker.typeCheckError(
            expr.pos,
            "Assignment to undeclared variable '" & identifier & "'",
            "use 'let' or 'var' to declare it",
          )
        else:
          let symbol = symbolOpt.get()
          let valueType = value.exprType
          discard areTypesCompatible(
            checker, scope, symbol.declStmt.varDeclStmt.typeAnnotation, valueType,
            expr.pos,
          )
      else:
        # For non-identifier left, just check type compatibility
        let valueType = value.exprType
        discard areTypesCompatible(checker, scope, leftType, valueType, expr.pos)

    analyzeTypeCheckingExpr(checker, scope, left)
    analyzeTypeCheckingExpr(checker, scope, value)
  of EkLogicalExpr:
    let leftType = expr.binaryOpExpr.left.exprType
    let rightType = expr.binaryOpExpr.right.exprType
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
    let leftType = expr.binaryOpExpr.left.exprType
    let rightType = expr.binaryOpExpr.right.exprType
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
    let leftType = expr.binaryOpExpr.left.exprType
    let rightType = expr.binaryOpExpr.right.exprType
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
    let operandType = expr.unaryOpExpr.operand.exprType
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
    let operandType = expr.addressOfExpr.operand.exprType
    if not operandType.hasAddress:
      checker.typeCheckError(
        expr.pos,
        "Address-of operator requires a value with an address",
        "but got " & $operandType,
      )
    analyzeTypeCheckingExpr(checker, scope, expr.addressOfExpr.operand)
  of EkDerefExpr:
    let operandType = expr.derefExpr.operand.exprType
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
        let parameters = funcSymbol.declStmt.funDeclStmt.parameters
        let hasVarArgs =
          parameters.len > 0 and parameters[^1].paramType.kind == TkMeta and
          parameters[^1].paramType.metaKind == MkCVarArgs
        if hasVarArgs:
          if arguments.len < parameters.len - 1:
            checker.typeCheckError(
              expr.pos,
              "Too few arguments to function '" & funcName & "'",
              "Expected at least " & $(parameters.len - 1) & " arguments but got " &
                $arguments.len,
            )
        if not hasVarArgs:
          if arguments.len < parameters.len:
            checker.typeCheckError(
              expr.pos,
              "Too few arguments to function '" & funcName & "'",
              "Expected " & $parameters.len & " arguments but got " & $arguments.len,
            )
          elif arguments.len > parameters.len:
            checker.typeCheckError(
              expr.pos,
              "Too many arguments to function '" & funcName & "'",
              "Expected " & $parameters.len & " arguments but got " & $arguments.len,
            )
        let regularParamCount =
          if hasVarArgs:
            parameters.len - 1
          else:
            parameters.len
        for i in 0 ..< min(regularParamCount, arguments.len):
          let argType = arguments[i].exprType
          let parameter = parameters[i]
          discard areTypesCompatible(
            checker, scope, parameter.paramType, argType, arguments[i].pos
          )
    analyzeTypeCheckingExpr(checker, scope, callee)
    for arg in arguments:
      analyzeTypeCheckingExpr(checker, scope, arg)
  of EkGroupExpr:
    analyzeTypeCheckingExpr(checker, scope, expr.groupExpr.expression)
  of EkStructLiteral:
    let structLit = expr.structLiteralExpr
    let typeName = structLit.typeName
    let symbolOpt = scope.findSymbol(typeName, expr.pos, Type)
    if symbolOpt.isNone:
      checker.typeCheckError(
        expr.pos,
        "Unknown struct type '" & typeName & "'",
        "Declare the struct type before using it",
      )
    else:
      let structType = symbolOpt.get().typeRepr
      if structType.kind != TkStruct:
        checker.typeCheckError(
          expr.pos, "Type '" & typeName & "' is not a struct type", ""
        )
      else:
        let members = structType.structType.members
        var seen = initTable[string, bool]()
        for m in structLit.members:
          if m.name notin members:
            checker.typeCheckError(
              m.namePos,
              "Unknown member '" & m.name & "' in struct literal",
              "Check struct definition for valid members",
            )
          else:
            let found = members[m.name]
            let memberType = found.typ
            let valueType = m.value.exprType
            discard
              areTypesCompatible(checker, scope, memberType, valueType, m.value.pos)
            seen[m.name] = true
            analyzeTypeCheckingExpr(checker, scope, m.value)
        # Check for missing required members (no default and not present)
        for _, m in members:
          if not seen.hasKey(m.name):
            checker.typeCheckError(
              expr.pos, "Missing member '" & m.name & "' in struct literal", ""
            )
  # Leaf nodes
  of EkIdentifier, EkIntLiteral, EkUIntLiteral, EkFloatLiteral, EkStringLiteral,
      EkCStringLiteral, EkCharLiteral, EkBoolLiteral, EkNilLiteral:
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
      let initType = initExpr.exprType
      discard
        areTypesCompatible(checker, scope, varDecl.typeAnnotation, initType, stmt.pos)
      analyzeTypeCheckingExpr(checker, scope, initExpr)
  of SkFunDecl:
    let funDecl = stmt.funDeclStmt
    if funDecl.body.isSome:
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeTypeChecking(checker, functionScope, funDecl.body.get())
  of SkTypeDecl:
    # No type checking needed for type declarations
    discard
  of SkStructDecl:
    # No type checking needed for struct declarations
    discard
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
        let exprType = returnExpr.exprType
        discard areTypesCompatible(
          checker, scope, funcSymbol.declStmt.funDeclStmt.returnType, exprType, stmt.pos
        )
      else:
        checker.typeCheckError(
          stmt.pos, "Return statement outside of function",
          "Return statements must be inside a function",
        )
      analyzeTypeCheckingExpr(checker, scope, returnExpr)
  of SkIfStmt:
    for i, branch in stmt.ifStmt.branches:
      let branchScope = scope.children[branch.scopeId]
      let condType = branch.condition.exprType
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
  of SkWhileStmt:
    let whileStmt = stmt.whileStmt
    let whileScope = scope.children[whileStmt.scopeId]
    let condType = whileStmt.condition.exprType
    if condType.kind != TkPrimitive or condType.primitive != Bool:
      checker.typeCheckError(
        whileStmt.condition.pos,
        "While condition must be a boolean expression",
        "but got " & $condType,
      )
    analyzeTypeCheckingExpr(checker, scope, whileStmt.condition)
    analyzeTypeChecking(checker, whileScope, whileStmt.body)
  of SkNop:
    discard
