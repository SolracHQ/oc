import ../types/[file_info, position, scope, ast, types, token]
import ../reporter
import std/options
import std/tables

# Helper functions for type inference
proc isIntFamily*(typ: Type): bool =
  ## Returns true if the type is from the signed integer family
  (typ.kind == TkPrimitive and typ.primitive in {Int, Int8, Int16, Int32, Int64}) or
    (typ.kind == TkMeta and typ.metaKind == MkIntInfer)

proc isUIntFamily*(typ: Type): bool =
  ## Returns true if the type is from the unsigned integer family
  (typ.kind == TkPrimitive and typ.primitive in {UInt, UInt8, UInt16, UInt32, UInt64}) or
    (typ.kind == TkMeta and typ.metaKind == MkUIntInfer)

proc isFloatFamily*(typ: Type): bool =
  ## Returns true if the type is from the floating-point family
  (typ.kind == TkPrimitive and typ.primitive in {Float, Float32, Float64}) or
    (typ.kind == TkMeta and typ.metaKind == MkFloatInfer)

proc isConcreteType*(typ: Type): bool =
  ## Returns true if the type is concrete (not inferred)
  typ.kind == TkPrimitive or (
    typ.kind == TkMeta and
    typ.metaKind notin {MkIntInfer, MkFloatInfer, MkUIntInfer, MkToInfer}
  )

proc resolveInferredType*(typ: Type): Type =
  ## Resolves inferred types to their concrete counterparts
  case typ.kind
  of TkMeta:
    case typ.metaKind
    of MkIntInfer:
      result = Type(kind: TkPrimitive, primitive: Int)
    of MkFloatInfer:
      result = Type(kind: TkPrimitive, primitive: Float)
    of MkUIntInfer:
      result = Type(kind: TkPrimitive, primitive: UInt)
    else:
      result = typ
  else:
    result = typ

type TypeInferencer* = ref object
  fileInfo*: FileInfo
  hasError*: bool

proc inferenceError*(
    inferencer: TypeInferencer, position: Position, msg: string, hint: string = ""
) =
  ## Prints an error message for the type inferencer
  logError("TypeInferencer", msg, position)
  inferencer.hasError = true

proc newTypeInferencer*(fileInfo: FileInfo): TypeInferencer =
  ## Creates a new type inferencer for the given file
  result = TypeInferencer()
  result.fileInfo = fileInfo
  result.hasError = false

proc inferExpressionType*(inferencer: TypeInferencer, scope: Scope, expr: Expr): Type =
  ## Infers the type of an expression node
  case expr.kind
  of EkIntLiteral:
    result = Type(kind: TkMeta, metaKind: MkIntInfer)
  of EkUIntLiteral:
    result = Type(kind: TkMeta, metaKind: MkUIntInfer)
  of EkFloatLiteral:
    result = Type(kind: TkMeta, metaKind: MkFloatInfer)
  of EkStringLiteral:
    result = Type(kind: TkPrimitive, primitive: String)
  of EkCStringLiteral:
    result = Type(kind: TkPrimitive, primitive: CString)
  of EkCharLiteral:
    result = Type(kind: TkPrimitive, primitive: Char)
  of EkBoolLiteral:
    result = Type(kind: TkPrimitive, primitive: Bool)
  of EkNilLiteral:
    result = Type(kind: TkMeta, metaKind: MkUnresolved, name: "nil")
  of EkIdentifier:
    let identifier = expr.identifierExpr.name
    let symbolOpt = scope.findSymbol(identifier, expr.pos, AnySymbol)
    if symbolOpt.isNone:
      inferencer.inferenceError(
        expr.pos, "Cannot infer type: undeclared identifier '" & identifier & "'"
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "undeclared_identifier")
    else:
      let symbol = symbolOpt.get()
      case symbol.kind
      of Variable:
        if not symbol.isInitialized and not symbol.isReadOnly:
          inferencer.inferenceError(
            expr.pos,
            "Cannot infer type from uninitialized variable '" & identifier & "'",
          )
          result = Type(kind: TkMeta, metaKind: MkResolveError, name: "uninitialized_variable")
        else:
          result = symbol.varType
      of Function:
        inferencer.inferenceError(
          expr.pos,
          "Cannot use function '" & identifier & "' as a value",
          "This will be supported in the future.",
        )
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "function_as_value")
      of Type:
        result = symbol.typeRepr
  of EkFunctionCall:
    let callee = expr.functionCallExpr.callee
    if callee.kind == EkIdentifier:
      let funcName = callee.identifierExpr.name
      let symbolOpt = scope.findSymbol(funcName, callee.pos, {Function, Variable})
      if symbolOpt.isNone:
        inferencer.inferenceError(
          expr.pos, "Call to undefined function '" & funcName & "'"
        )
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "undefined_function")
      elif symbolOpt.get().kind != Function:
        inferencer.inferenceError(
          expr.pos,
          "Cannot call non-function '" & funcName & "'",
          "callable types are not yet supported",
        )
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_function_call")
      else:
        result = symbolOpt.get().returnType
    else:
      inferencer.inferenceError(expr.pos, "Complex function calls not yet supported")
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "unsupported_call")
  of EkGroupExpr:
    result = inferExpressionType(inferencer, scope, expr.groupExpr.expression)
  of EkUnaryExpr:
    let operand = inferExpressionType(inferencer, scope, expr.unaryOpExpr.operand)
    case expr.unaryOpExpr.operator
    of TkMinus:
      if operand.isIntFamily or operand.isFloatFamily or operand.isUIntFamily:
        result = operand
      else:
        inferencer.inferenceError(
          expr.pos, "Unary " & $expr.unaryOpExpr.operator & " requires numeric operand"
        )
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_mismatch")
    of TkBang:
      if operand.kind == TkPrimitive and operand.primitive == Bool:
        result = Type(kind: TkPrimitive, primitive: Bool)
      else:
        inferencer.inferenceError(expr.pos, "Logical negation requires boolean operand")
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_boolean_negation")
    else:
      inferencer.inferenceError(
        expr.pos, "Unsupported unary operator: " & $expr.unaryOpExpr.operator
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "unsupported_unary")
  of EkLogicalExpr:
    let leftType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.left)
    let rightType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.right)
    if leftType.kind != TkPrimitive or leftType.primitive != Bool:
      inferencer.inferenceError(
        expr.binaryOpExpr.left.pos, "Logical expression requires boolean operands"
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_boolean_logical")
    elif rightType.kind != TkPrimitive or rightType.primitive != Bool:
      inferencer.inferenceError(
        expr.binaryOpExpr.right.pos, "Logical expression requires boolean operands"
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_boolean_logical")
    else:
      result = Type(kind: TkPrimitive, primitive: Bool)
  of EkEqualityExpr:
    let leftType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.left)
    let rightType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.right)
    result = Type(kind: TkPrimitive, primitive: Bool)
  of EkComparisonExpr:
    let leftType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.left)
    let rightType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.right)
    if leftType == rightType:
      result = Type(kind: TkPrimitive, primitive: Bool)
    elif (isIntFamily(leftType) and isIntFamily(rightType)) or
        (isFloatFamily(leftType) and isFloatFamily(rightType)) or
        (isUIntFamily(leftType) and isUIntFamily(rightType)):
      result = Type(kind: TkPrimitive, primitive: Bool)
    else:
      inferencer.inferenceError(
        expr.pos,
        "Type mismatch in comparison: " & $leftType & " cannot be compared with " &
          $rightType,
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "comparison_type_mismatch")
  of EkAdditiveExpr, EkMultiplicativeExpr:
    let leftType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.left)
    let rightType = inferExpressionType(inferencer, scope, expr.binaryOpExpr.right)
    if leftType.kind == TkMeta and rightType.kind == TkMeta and
        leftType.metaKind == rightType.metaKind and
        leftType.metaKind in {MkIntInfer, MkFloatInfer, MkUIntInfer}:
      result = leftType
    elif isIntFamily(leftType) and isIntFamily(rightType):
      if isConcreteType(leftType) and isConcreteType(rightType):
        if leftType.primitive == rightType.primitive:
          result = leftType
        else:
          inferencer.inferenceError(
            expr.pos,
            "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
              " (implicit conversion not allowed)",
          )
          result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_mismatch")
      elif isConcreteType(leftType):
        result = leftType
      else:
        result = rightType
    elif isFloatFamily(leftType) and isFloatFamily(rightType):
      if isConcreteType(leftType) and isConcreteType(rightType):
        if leftType.primitive == rightType.primitive:
          result = leftType
        else:
          inferencer.inferenceError(
            expr.pos,
            "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
              " (implicit conversion not allowed)",
          )
          result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_mismatch")
      elif isConcreteType(leftType):
        result = leftType
      else:
        result = rightType
    elif (isIntFamily(leftType) and isFloatFamily(rightType)) or
        (isFloatFamily(leftType) and isIntFamily(rightType)):
      inferencer.inferenceError(
        expr.pos,
        "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
          " (int and float types cannot be mixed)",
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_family_mismatch")
    elif (isIntFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isIntFamily(rightType)):
      inferencer.inferenceError(
        expr.pos,
        "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
          " (int and uint types cannot be mixed)",
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_family_mismatch")
    elif (isFloatFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isFloatFamily(rightType)):
      inferencer.inferenceError(
        expr.pos,
        "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
          " (float and uint types cannot be mixed)",
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_family_mismatch")
    else:
      inferencer.inferenceError(expr.pos, "Arithmetic operations require numeric types")
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_numeric_arithmetic")
  of EkAssignment:
    result = inferExpressionType(inferencer, scope, expr.assignmentExpr.value)
  of EkMemberAccess:
    inferencer.inferenceError(
      expr.pos, "Member access type inference not yet supported"
    )
    result = Type(kind: TkMeta, metaKind: MkResolveError, name: "unsupported_member_access")
  of EkAddressOfExpr:
    let operand = expr.addressOfExpr.operand
    if operand.kind != EkIdentifier:
      inferencer.inferenceError(expr.pos, "Address-of operator requires variable")
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "address_of_non_variable")
    let symbol = scope.findSymbol(operand.identifierExpr.name, expr.pos, Variable)
    if symbol.isNone:
      inferencer.inferenceError(
        expr.pos,
        "Cannot take address of undeclared identifier '" & operand.identifierExpr.name & "'",
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "address_of_undeclared")
    let operandType = inferExpressionType(inferencer, scope, operand)
    if operandType.kind == TkMeta:
      inferencer.inferenceError(expr.pos, "Cannot take address of a variable with a Meta Value", "This should never happen")
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "address_of_non_variable")
    else:
      result = newPointerType(operandType, symbol.get().isReadOnly)
  of EkDerefExpr:
    let operandType = inferExpressionType(inferencer, scope, expr.derefExpr.operand)
    if operandType.kind == TkPointer:
      result = operandType.pointerTo[]
    else:
      inferencer.inferenceError(expr.pos, "Dereference operator requires pointer type")
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "dereference_non_pointer")
  of EkType:
    result = expr.typeExpr

proc analyzeTypeInference*(inferencer: TypeInferencer, scope: Scope, stmt: Stmt) =
  ## Infer type of variables based on their expressions
  ## Only process var/let declarations with MkToInfer type and assignments
  case stmt.kind
  of SkModule:
    for s in stmt.moduleStmt.statements:
      analyzeTypeInference(inferencer, scope, s)
  of SkVarDecl:
    let varDecl = stmt.varDeclStmt
    if varDecl.typeAnnotation.kind == TkMeta and
        varDecl.typeAnnotation.metaKind == MkToInfer:
      if varDecl.initializer.isNone:
        inferencer.inferenceError(
          stmt.pos,
          "Cannot infer type for variable '" & varDecl.identifier &
            "' without initializer",
        )
      else:
        var inferredType =
          inferExpressionType(inferencer, scope, varDecl.initializer.get())
        if inferredType.kind == TkMeta:
          case inferredType.metaKind
          of MkIntInfer:
            inferredType = Type(kind: TkPrimitive, primitive: Int)
          of MkFloatInfer:
            inferredType = Type(kind: TkPrimitive, primitive: Float)
          of MkUIntInfer:
            inferredType = Type(kind: TkPrimitive, primitive: UInt)
          else:
            discard
        let symbolOpt = scope.findSymbol(varDecl.identifier, stmt.pos, Variable)
        if symbolOpt.isSome:
          inferredType.hasAddress = true
          symbolOpt.get().varType = inferredType
          stmt.varDeclStmt.typeAnnotation = inferredType
        else:
          inferencer.inferenceError(
            stmt.pos,
            "Internal error: symbol '" & varDecl.identifier & "' not found in scope",
          )
  of SkExprStmt: discard
  of SkReturnStmt: discard
  of SkFunDecl:
    let funDecl = stmt.funDeclStmt
    if funDecl.body.isSome:
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeTypeInference(inferencer, functionScope, funDecl.body.get())
  of SkBlockStmt:
    let blockScope = scope.children[stmt.blockStmt.blockId]
    for s in stmt.blockStmt.statements:
      analyzeTypeInference(inferencer, blockScope, s)
  of SkIfStmt:
    for branch in stmt.ifStmt.branches:
      let branchScope = scope.children[branch.scopeId]
      analyzeTypeInference(inferencer, branchScope, branch.body)
    if stmt.ifStmt.elseBranch.isSome:
      let elseBranch = stmt.ifStmt.elseBranch.get()
      let elseScope = scope.children[elseBranch.scopeId]
      analyzeTypeInference(inferencer, elseScope, elseBranch.body)
  of SkNop:
    discard

# Expression analysis for assignment and other expressions
proc analyzeTypeInference*(inferencer: TypeInferencer, scope: Scope, expr: Expr) =
  case expr.kind
  of EkAssignment:
    let identifier = expr.assignmentExpr.identifier
    var valueType = inferExpressionType(inferencer, scope, expr.assignmentExpr.value)
    let symbolOpt = scope.findSymbol(identifier, expr.pos, Variable)
    if symbolOpt.isSome:
      let symbol = symbolOpt.get()
      if symbol.varType.kind == TkMeta and symbol.varType.metaKind == MkToInfer:
        if valueType.kind == TkMeta:
          case valueType.metaKind
          of MkIntInfer:
            valueType = Type(kind: TkPrimitive, primitive: Int)
          of MkFloatInfer:
            valueType = Type(kind: TkPrimitive, primitive: Float)
          of MkUIntInfer:
            valueType = Type(kind: TkPrimitive, primitive: UInt)
          else:
            discard
        symbol.varType = valueType
      elif (isIntFamily(symbol.varType) and isFloatFamily(valueType)) or
          (isFloatFamily(symbol.varType) and isIntFamily(valueType)):
        inferencer.inferenceError(
          expr.pos,
          "Type mismatch in assignment: cannot assign " & $valueType & " to " &
            $symbol.varType,
        )
    analyzeTypeInference(inferencer, scope, expr.assignmentExpr.value)
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr, EkMultiplicativeExpr:
    analyzeTypeInference(inferencer, scope, expr.binaryOpExpr.left)
    analyzeTypeInference(inferencer, scope, expr.binaryOpExpr.right)
  of EkUnaryExpr:
    analyzeTypeInference(inferencer, scope, expr.unaryOpExpr.operand)
  of EkAddressOfExpr:
    analyzeTypeInference(inferencer, scope, expr.addressOfExpr.operand)
  of EkDerefExpr:
    analyzeTypeInference(inferencer, scope, expr.derefExpr.operand)
  of EkMemberAccess:
    analyzeTypeInference(inferencer, scope, expr.memberAccessExpr.obj)
  of EkGroupExpr:
    analyzeTypeInference(inferencer, scope, expr.groupExpr.expression)
  of EkFunctionCall:
    analyzeTypeInference(inferencer, scope, expr.functionCallExpr.callee)
    for arg in expr.functionCallExpr.arguments:
      analyzeTypeInference(inferencer, scope, arg)
  # Literals and identifiers do not need further analysis
  of EkIntLiteral, EkIdentifier, EkUIntLiteral, EkFloatLiteral, EkStringLiteral,
      EkCStringLiteral, EkCharLiteral, EkBoolLiteral, EkNilLiteral, EkType:
    discard
