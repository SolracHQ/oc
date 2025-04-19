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

proc analyzeTypeChecking*(checker: TypeChecker, scope: Scope, node: Node) =
  ## Check that all types match correctly in expressions, assignments, and function calls
  case node.kind
  of NkModule:
    # Process each statement in the module
    for stmt in node.moduleNode.statements:
      analyzeTypeChecking(checker, scope, stmt)
  of NkVarDecl:
    # Check type of initializer against variable type
    let varDecl = node.varDeclNode
    if varDecl.initializer.isSome:
      # Verify that the initializer type matches the variable type
      let initExpr = varDecl.initializer.get()
      let inferencer = newTypeInferencer(checker.fileInfo)
      let initType = inferExpressionType(inferencer, scope, initExpr)
      discard areTypesCompatible(checker, varDecl.typeAnnotation, initType, node.pos)

      # Process the initializer expression
      analyzeTypeChecking(checker, scope, initExpr)
  of NkAssignment:
    # Check type of right side against left side variable type
    let identifier = node.assignmentNode.identifier
    let value = node.assignmentNode.value

    # Get variable type
    let symbolOpt = scope.findSymbol(identifier, node.pos, Variable)
    if symbolOpt.isNone:
      checker.typeCheckError(
        node.pos,
        "Assignment to undeclared variable '" & identifier & "'",
        "use 'let' or 'var' to declare it",
      )
    else:
      let symbol = symbolOpt.get()
      let inferencer = newTypeInferencer(checker.fileInfo)
      let valueType = inferExpressionType(inferencer, scope, value)
      discard areTypesCompatible(checker, symbol.varType, valueType, node.pos)

    # Process value expression
    analyzeTypeChecking(checker, scope, value)
  of NkFunctionCall:
    # Check argument types against function parameter types
    let callee = node.functionCallNode.callee
    let arguments = node.functionCallNode.arguments

    # Get function information
    if callee.kind == NkIdentifier:
      let funcName = callee.identifierNode.name
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

        # Handle varargs - check if the last parameter is a varargs type
        let hasVarArgs =
          paramTypes.len > 0 and paramTypes[^1].kind == TkMeta and
          paramTypes[^1].metaKind == MkCVarArgs

        # Check number of arguments with varargs handling
        if hasVarArgs:
          if arguments.len < paramTypes.len - 1:
            checker.typeCheckError(
              node.pos,
              "Too few arguments to function '" & funcName & "'",
              "Expected at least " & $(paramTypes.len - 1) & " arguments but got " &
                $arguments.len,
            )
        if not hasVarArgs:
          if arguments.len < paramTypes.len:
            # Regular function without varargs
            checker.typeCheckError(
              node.pos,
              "Too few arguments to function '" & funcName & "'",
              "Expected " & $paramTypes.len & " arguments but got " & $arguments.len,
            )
          elif arguments.len > paramTypes.len:
            # Too many arguments for a regular function
            checker.typeCheckError(
              node.pos,
              "Too many arguments to function '" & funcName & "'",
              "Expected " & $paramTypes.len & " arguments but got " & $arguments.len,
            )

        # Check each argument type
        let inferencer = newTypeInferencer(checker.fileInfo)
        let regularParamCount =
          if hasVarArgs:
            paramTypes.len - 1
          else:
            paramTypes.len

        # Check regular parameters
        for i in 0 ..< min(regularParamCount, arguments.len):
          let argType = inferExpressionType(inferencer, scope, arguments[i])
          let paramType = paramTypes[i]
          discard areTypesCompatible(checker, paramType, argType, arguments[i].pos)

    # Process the callee and all arguments
    analyzeTypeChecking(checker, scope, callee)
    for arg in arguments:
      analyzeTypeChecking(checker, scope, arg)
  of NkReturnStmt:
    # Check that the return type matches the function's declared return type
    if node.returnStmtNode.expression.isSome:
      let returnExpr = node.returnStmtNode.expression.get()

      # Find the enclosing function
      var currentScope = scope
      var funcSymbol: Symbol = nil

      while currentScope != nil:
        if currentScope.kind == FunctionScope:
          # Found the enclosing function
          let parentScope = currentScope.parent
          if parentScope != nil:
            let funcName = currentScope.name
            let symbolOpt = parentScope.findSymbol(funcName, node.pos, Function)
            if symbolOpt.isSome:
              funcSymbol = symbolOpt.get()
          break
        currentScope = currentScope.parent

      if funcSymbol != nil:
        # Check return type compatibility
        let inferencer = newTypeInferencer(checker.fileInfo)
        let exprType = inferExpressionType(inferencer, scope, returnExpr)
        discard areTypesCompatible(checker, funcSymbol.returnType, exprType, node.pos)
      else:
        checker.typeCheckError(
          node.pos,
          "Return statement outside of function",
          "Return statements must be inside a function",
        )

      # Process the return expression
      analyzeTypeChecking(checker, scope, returnExpr)
  of NkFunDecl:
    # Process function body if present
    let funDecl = node.funDeclNode
    if funDecl.body.isSome:
      # Use function scope for analyzing the body
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeTypeChecking(checker, functionScope, funDecl.body.get())
  of NkBlockStmt:
    # Process statements in block scope
    let blockScope = scope.children[node.blockStmtNode.blockId]
    for stmt in node.blockStmtNode.statements:
      analyzeTypeChecking(checker, blockScope, stmt)
  of NkExprStmt:
    # Process expression statements
    analyzeTypeChecking(checker, scope, node.exprStmtNode.expression)
  of NkIfStmt:
    # Check both branches of the if statement
    for i, branch in node.ifStmtNode.branches:
      let branchScope = scope.children[branch.scopeId]
      let inferencer = newTypeInferencer(checker.fileInfo)
      let condType = inferExpressionType(inferencer, scope, branch.condition)
      if condType.kind != TkPrimitive or condType.primitive != Bool:
        checker.typeCheckError(
          branch.condition.pos,
          if i == 0:
            "if"
          else:
            "elif" & " condition must be a boolean expression",
          "but got " & $condType,
        )
      analyzeTypeChecking(checker, branchScope, branch.condition)
      analyzeTypeChecking(checker, branchScope, branch.body)
    if node.ifStmtNode.elseBranch.isSome:
      let elseBranch = node.ifStmtNode.elseBranch.get
      let elseScope = scope.children[elseBranch.scopeId]
      analyzeTypeChecking(checker, elseScope, elseBranch.body)
  of NkLogicalExpr:
    # Check both operands are boolean
    let inferencer = newTypeInferencer(checker.fileInfo)
    let leftType = inferExpressionType(inferencer, scope, node.binaryOpNode.left)
    let rightType = inferExpressionType(inferencer, scope, node.binaryOpNode.right)

    if leftType.kind != TkPrimitive or leftType.primitive != Bool:
      checker.typeCheckError(
        node.binaryOpNode.left.pos,
        "Logical operator requires boolean operands",
        "left operand is " & $leftType,
      )

    if rightType.kind != TkPrimitive or rightType.primitive != Bool:
      checker.typeCheckError(
        node.binaryOpNode.right.pos,
        "Logical operator requires boolean operands",
        "right operand is " & $rightType,
      )

    # Process both operands
    analyzeTypeChecking(checker, scope, node.binaryOpNode.left)
    analyzeTypeChecking(checker, scope, node.binaryOpNode.right)
  of NkEqualityExpr, NkComparisonExpr:
    # Check both operands are of the same type
    let inferencer = newTypeInferencer(checker.fileInfo)
    let leftType = inferExpressionType(inferencer, scope, node.binaryOpNode.left)
    let rightType = inferExpressionType(inferencer, scope, node.binaryOpNode.right)

    if leftType.kind == TkPrimitive and rightType.kind == TkPrimitive:
      if leftType.primitive != rightType.primitive:
        checker.typeCheckError(
          node.pos,
          "Type mismatch in comparison.",
          $leftType.primitive & " cannot be compared with " & $rightType.primitive,
        )

    # Process both operands
    analyzeTypeChecking(checker, scope, node.binaryOpNode.left)
    analyzeTypeChecking(checker, scope, node.binaryOpNode.right)
  of NkAdditiveExpr, NkMultiplicativeExpr:
    # Check both operands are numeric and of the same type
    let inferencer = newTypeInferencer(checker.fileInfo)
    let leftType = inferExpressionType(inferencer, scope, node.binaryOpNode.left)
    let rightType = inferExpressionType(inferencer, scope, node.binaryOpNode.right)

    # Both are numeric but possibly different types
    if (isIntFamily(leftType) and isIntFamily(rightType)) or
        (isFloatFamily(leftType) and isFloatFamily(rightType)) or
        (isUIntFamily(leftType) and isUIntFamily(rightType)):
      # Types within the same family - OK
      discard
    elif (isIntFamily(leftType) and isFloatFamily(rightType)) or
        (isFloatFamily(leftType) and isIntFamily(rightType)):
      checker.typeCheckError(
        node.pos,
        "Type mismatch in arithmetic operation.",
        $leftType & " and " & $rightType & " (int and float types cannot be mixed)",
      )
    elif (isIntFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isIntFamily(rightType)):
      checker.typeCheckError(
        node.pos,
        "Type mismatch in arithmetic operation.",
        $leftType & " and " & $rightType & " (int and uint types cannot be mixed)",
      )
    elif (isFloatFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isFloatFamily(rightType)):
      checker.typeCheckError(
        node.pos,
        "Type mismatch in arithmetic operation.",
        $leftType & " and " & $rightType & " (float and uint types cannot be mixed)",
      )
    else:
      checker.typeCheckError(
        node.pos,
        "Arithmetic operations require numeric types",
        "but got " & $leftType & " and " & $rightType,
      )

    # Process both operands
    analyzeTypeChecking(checker, scope, node.binaryOpNode.left)
    analyzeTypeChecking(checker, scope, node.binaryOpNode.right)
  of NkUnaryExpr:
    # Check operand type based on operator
    let inferencer = newTypeInferencer(checker.fileInfo)
    let operandType = inferExpressionType(inferencer, scope, node.unaryOpNode.operand)

    case node.unaryOpNode.operator
    of TkMinus:
      # Negation requires numeric type
      if not (
        isIntFamily(operandType) or isFloatFamily(operandType) or
        isUIntFamily(operandType)
      ):
        checker.typeCheckError(
          node.pos, "Unary minus requires numeric operand", "but got " & $operandType
        )
    of TkBang:
      # Logical negation requires boolean
      if operandType.kind != TkPrimitive or operandType.primitive != Bool:
        checker.typeCheckError(
          node.pos,
          "Logical negation requires boolean operand",
          "but got " & $operandType,
        )
    else:
      checker.typeCheckError(
        node.pos,
        "Unsupported unary operator: " & $node.unaryOpNode.operator,
        "THis should not happen: " & $node.unaryOpNode.operator,
      )

    # Process the operand
    analyzeTypeChecking(checker, scope, node.unaryOpNode.operand)
  of NkAddressOfExpr:
    # Check operand type
    let inferencer = newTypeInferencer(checker.fileInfo)
    let operandType =
      inferExpressionType(inferencer, scope, node.addressOfExprNode.operand)

    # Address-of operator requires a variable
    if not operandType.hasAddress:
      checker.typeCheckError(
        node.pos,
        "Address-of operator requires a value with an address",
        "but got " & $operandType,
      )

    # Process the operand
    analyzeTypeChecking(checker, scope, node.addressOfExprNode.operand)
  of NkDerefExpr:
    # Check operand type
    let inferencer = newTypeInferencer(checker.fileInfo)
    let operandType = inferExpressionType(inferencer, scope, node.derefExprNode.operand)

    # Dereference operator requires a pointer type
    if operandType.kind != TkPointer and operandType.kind != TkROPointer:
      checker.typeCheckError(
        node.pos,
        "Dereference operator requires a pointer type",
        "but got " & $operandType,
      )

    # Process the operand
    analyzeTypeChecking(checker, scope, node.derefExprNode.operand)
  of NkMemberAccess:
    # Process the object
    analyzeTypeChecking(checker, scope, node.memberAccessNode.obj)
  of NkGroupExpr:
    # Process the inner expression
    analyzeTypeChecking(checker, scope, node.groupNode.expression)

  # Leaf nodes - no further processing needed
  of NkIdentifier, NkIntLiteral, NkUIntLiteral, NkFloatLiteral, NkStringLiteral,
      NkCStringLiteral, NkCharLiteral, NkBoolLiteral, NkNilLiteral, NkType, NkNop:
    discard
