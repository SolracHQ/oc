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

proc inferenceError*(inferencer: TypeInferencer, position: Position, msg: string) =
  ## Prints an error message for the type inferencer
  logError("TypeInferencer", msg, position)
  inferencer.hasError = true

proc newTypeInferencer*(fileInfo: FileInfo): TypeInferencer =
  ## Creates a new type inferencer for the given file
  result = TypeInferencer()
  result.fileInfo = fileInfo
  result.hasError = false

proc inferExpressionType*(inferencer: TypeInferencer, scope: Scope, node: Node): Type =
  ## Infers the type of an expression node
  case node.kind
  of NkIntLiteral:
    # Integer literals are inferred as general Int type
    result = Type(kind: TkMeta, metaKind: MkIntInfer)
  of NkUIntLiteral:
    # Unsigned integer literals are inferred as general UInt type
    result = Type(kind: TkMeta, metaKind: MkUIntInfer)
  of NkFloatLiteral:
    result = Type(kind: TkMeta, metaKind: MkFloatInfer)
  of NkStringLiteral:
    result = Type(kind: TkPrimitive, primitive: String)
  of NkCStringLiteral:
    # C-string literals have the CString type
    result = Type(kind: TkPrimitive, primitive: CString)
  of NkCharLiteral:
    result = Type(kind: TkPrimitive, primitive: Char)
  of NkBoolLiteral:
    result = Type(kind: TkPrimitive, primitive: Bool)
  of NkNilLiteral:
    # Nil has a special type
    result = Type(kind: TkMeta, metaKind: MkUnresolved, name: "nil")
  of NkIdentifier:
    # For identifiers, look up their type in the symbol table
    let identifier = node.identifierNode.name
    let symbolOpt = scope.findSymbol(identifier, node.pos)

    if symbolOpt.isNone:
      inferencer.inferenceError(
        node.pos, "Cannot infer type: undeclared identifier '" & identifier & "'"
      )
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "undeclared_identifier")
    else:
      let symbol = symbolOpt.get()
      case symbol.kind
      of Variable:
        if not symbol.isInitialized and not symbol.isReadOnly:
          inferencer.inferenceError(
            node.pos,
            "Cannot infer type from uninitialized variable '" & identifier & "'",
          )
          result =
            Type(kind: TkMeta, metaKind: MkResolveError, name: "uninitialized_variable")
        else:
          result = symbol.varType
      of Function:
        inferencer.inferenceError(
          node.pos, "Cannot use function '" & identifier & "' as a value"
        )
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "function_as_value")
      of Type:
        result = symbol.typeRepr
  of NkFunctionCall:
    # For function calls, look up the function and get its return type
    let callee = node.functionCallNode.callee
    if callee.kind == NkIdentifier:
      let funcName = callee.identifierNode.name
      let symbolOpt = scope.findSymbol(funcName, callee.pos)

      if symbolOpt.isNone:
        inferencer.inferenceError(
          node.pos, "Call to undefined function '" & funcName & "'"
        )
        result =
          Type(kind: TkMeta, metaKind: MkResolveError, name: "undefined_function")
      elif symbolOpt.get().kind != Function:
        inferencer.inferenceError(
          node.pos, "Cannot call non-function '" & funcName & "'"
        )
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_function_call")
      else:
        # Get return type of the function
        result = symbolOpt.get().returnType
    else:
      inferencer.inferenceError(node.pos, "Complex function calls not yet supported")
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "unsupported_call")
  of NkGroupExpr:
    # For grouped expressions, infer the type of the inner expression
    result = inferExpressionType(inferencer, scope, node.groupNode.expression)
  of NkUnaryExpr:
    # For unary expressions, infer based on the operator and operand
    let operand = inferExpressionType(inferencer, scope, node.unaryOpNode.operand)
    case node.unaryOpNode.operator
    of TkMinus:
      # For numeric negation/identity, preserve the numeric type
      if operand.kind == TkPrimitive and
          operand.primitive in {
            Int, Int8, Int16, Int32, Int64, UInt, UInt8, UInt16, UInt32, UInt64, Float,
            Float32, Float64,
          }:
        result = operand
      else:
        inferencer.inferenceError(
          node.pos, "Unary " & $node.unaryOpNode.operator & " requires numeric operand"
        )
        result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_mismatch")
    of TkBang:
      # Logical negation requires Bool operand type and results in Bool
      if operand.kind == TkPrimitive and operand.primitive == Bool:
        result = Type(kind: TkPrimitive, primitive: Bool)
      else:
        inferencer.inferenceError(node.pos, "Logical negation requires boolean operand")
        result =
          Type(kind: TkMeta, metaKind: MkResolveError, name: "non_boolean_negation")
    else:
      inferencer.inferenceError(
        node.pos, "Unsupported unary operator: " & $node.unaryOpNode.operator
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "unsupported_unary")
  of NkLogicalExpr:
    # Logical expressions require bool operands and result in bool
    let leftType = inferExpressionType(inferencer, scope, node.binaryOpNode.left)
    let rightType = inferExpressionType(inferencer, scope, node.binaryOpNode.right)

    if leftType.kind != TkPrimitive or leftType.primitive != Bool:
      inferencer.inferenceError(
        node.binaryOpNode.left.pos, "Logical expression requires boolean operands"
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_boolean_logical")
    elif rightType.kind != TkPrimitive or rightType.primitive != Bool:
      inferencer.inferenceError(
        node.binaryOpNode.right.pos, "Logical expression requires boolean operands"
      )
      result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_boolean_logical")
    else:
      result = Type(kind: TkPrimitive, primitive: Bool)
  of NkEqualityExpr:
    # Equality expressions always result in Bool, but types must be compatible
    let leftType = inferExpressionType(inferencer, scope, node.binaryOpNode.left)
    let rightType = inferExpressionType(inferencer, scope, node.binaryOpNode.right)
    result = Type(kind: TkPrimitive, primitive: Bool)
  of NkComparisonExpr:
    # Comparison expressions require matching types or compatible meta-inferred types
    let leftType = inferExpressionType(inferencer, scope, node.binaryOpNode.left)
    let rightType = inferExpressionType(inferencer, scope, node.binaryOpNode.right)
    # Allow if both are the same meta-inferred family
    if leftType == rightType:
      result = Type(kind: TkPrimitive, primitive: Bool)
    elif (isIntFamily(leftType) and isIntFamily(rightType)) or
        (isFloatFamily(leftType) and isFloatFamily(rightType)) or
        (isUIntFamily(leftType) and isUIntFamily(rightType)):
      result = Type(kind: TkPrimitive, primitive: Bool)
    else:
      inferencer.inferenceError(
        node.pos,
        "Type mismatch in comparison: " & $leftType & " cannot be compared with " &
          $rightType,
      )
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "comparison_type_mismatch")
  of NkAdditiveExpr, NkMultiplicativeExpr:
    # Handle binary operations with inferred and concrete types
    let leftType = inferExpressionType(inferencer, scope, node.binaryOpNode.left)
    let rightType = inferExpressionType(inferencer, scope, node.binaryOpNode.right)

    # Both are the same inferred type - return that inferred type
    if leftType.kind == TkMeta and rightType.kind == TkMeta and
        leftType.metaKind == rightType.metaKind and
        leftType.metaKind in {MkIntInfer, MkFloatInfer, MkUIntInfer}:
      result = leftType

    # Handle int family types (concrete or inferred)
    elif isIntFamily(leftType) and isIntFamily(rightType):
      # If both are concrete, they must be the same type
      if isConcreteType(leftType) and isConcreteType(rightType):
        if leftType.primitive == rightType.primitive:
          result = leftType
        else:
          inferencer.inferenceError(
            node.pos,
            "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
              " (implicit conversion not allowed)",
          )
          result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_mismatch")
      # If one is inferred and one is concrete, return the concrete type
      elif isConcreteType(leftType):
        result = leftType
      else:
        result = rightType

    # Handle float family types (concrete or inferred)
    elif isFloatFamily(leftType) and isFloatFamily(rightType):
      # If both are concrete, they must be the same type
      if isConcreteType(leftType) and isConcreteType(rightType):
        if leftType.primitive == rightType.primitive:
          result = leftType
        else:
          inferencer.inferenceError(
            node.pos,
            "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
              " (implicit conversion not allowed)",
          )
          result = Type(kind: TkMeta, metaKind: MkResolveError, name: "type_mismatch")
      # If one is inferred and one is concrete, return the concrete type
      elif isConcreteType(leftType):
        result = leftType
      else:
        result = rightType

    # Error case: mismatched type families (int vs float)
    elif (isIntFamily(leftType) and isFloatFamily(rightType)) or
        (isFloatFamily(leftType) and isIntFamily(rightType)):
      inferencer.inferenceError(
        node.pos,
        "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
          " (int and float types cannot be mixed)",
      )
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "type_family_mismatch")

    # Error case: mismatched type families (int vs uint)
    elif (isIntFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isIntFamily(rightType)):
      inferencer.inferenceError(
        node.pos,
        "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
          " (int and uint types cannot be mixed)",
      )
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "type_family_mismatch")

    # Error case: mismatched type families (float vs uint)
    elif (isFloatFamily(leftType) and isUIntFamily(rightType)) or
        (isUIntFamily(leftType) and isFloatFamily(rightType)):
      inferencer.inferenceError(
        node.pos,
        "Type mismatch in arithmetic operation: " & $leftType & " and " & $rightType &
          " (float and uint types cannot be mixed)",
      )
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "type_family_mismatch")

    # Other error cases
    else:
      inferencer.inferenceError(node.pos, "Arithmetic operations require numeric types")
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "non_numeric_arithmetic")
  of NkAssignment:
    # For assignments, infer the type of the right-hand side
    result = inferExpressionType(inferencer, scope, node.assignmentNode.value)
  of NkMemberAccess:
    # Member access is more complex and will be implemented later
    inferencer.inferenceError(
      node.pos, "Member access type inference not yet supported"
    )
    result =
      Type(kind: TkMeta, metaKind: MkResolveError, name: "unsupported_member_access")
  of NkAddressOfExpr:
    # Address-of operator requires a variable
    let operand = node.addressOfExprNode.operand
    if operand.kind != NkIdentifier:
      inferencer.inferenceError(node.pos, "Address-of operator requires variable")
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "address_of_non_variable")
    let symbol = scope.findSymbol(operand.identifierNode.name, node.pos)
    if symbol.isNone:
      inferencer.inferenceError(
        node.pos,
        "Cannot take address of undeclared identifier '" & operand.identifierNode.name &
          "'",
      )
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "address_of_undeclared")
    else:
      # Check if the symbol is a variable
      if symbol.get().kind != Variable:
        inferencer.inferenceError(
          node.pos,
          "Cannot take address of non-variable '" & operand.identifierNode.name & "'",
        )
        result =
          Type(kind: TkMeta, metaKind: MkResolveError, name: "address_of_non_variable")
    let operandType = inferExpressionType(inferencer, scope, operand)
    if operandType.kind == TkMeta:
      inferencer.inferenceError(node.pos, "Cannot take address of non-variable")
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "address_of_non_variable")
    else:
      result = newPointerType(operandType, symbol.get().isReadOnly)
  of NkDerefExpr:
    # Dereference operator requires a pointer type
    let operandType = inferExpressionType(inferencer, scope, node.derefExprNode.operand)
    if operandType.kind == TkPointer:
      result = operandType.pointerTo[]
    else:
      inferencer.inferenceError(node.pos, "Dereference operator requires pointer type")
      result =
        Type(kind: TkMeta, metaKind: MkResolveError, name: "dereference_non_pointer")
  of NkType:
    result = node.typeNode
  of NkVarDecl, NkFunDecl, NkBlockStmt, NkExprStmt, NkReturnStmt, NkModule, NkNop,
      NkIfStmt:
    inferencer.inferenceError(
      node.pos, "Internal error: trying to infer type of non-expression"
    )
    result = Type(kind: TkMeta, metaKind: MkResolveError, name: "non_expression")

proc analyzeTypeInference*(inferencer: TypeInferencer, scope: Scope, node: Node) =
  ## Infer type of variables based on their expressions
  ## Only process var/let declarations with MkToInfer type and assignments
  case node.kind
  of NkModule:
    # Process each statement in the module
    for stmt in node.moduleNode.statements:
      analyzeTypeInference(inferencer, scope, stmt)
  of NkVarDecl:
    # Handle variable declarations that need type inference
    let varDecl = node.varDeclNode

    # Only infer if type is MkToInfer
    if varDecl.typeAnnotation.kind == TkMeta and
        varDecl.typeAnnotation.metaKind == MkToInfer:
      if varDecl.initializer.isNone:
        inferencer.inferenceError(
          node.pos,
          "Cannot infer type for variable '" & varDecl.identifier &
            "' without initializer",
        )
      else:
        # Infer type from initializer
        var inferredType =
          inferExpressionType(inferencer, scope, varDecl.initializer.get())

        # Resolve inferred family types to concrete types
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

        let symbolOpt = scope.findSymbol(varDecl.identifier, node.pos)
        if symbolOpt.isSome:
          if symbolOpt.get().kind != Variable:
            inferencer.inferenceError(
              node.pos,
              "Cannot assign type to non-variable '" & varDecl.identifier & "'",
            )
            return
          # if variable is const make the type const to
          inferredType.hasAddress = true
          symbolOpt.get().varType = inferredType
          node.varDeclNode.typeAnnotation = inferredType
        else:
          inferencer.inferenceError(
            node.pos,
            "Internal error: symbol '" & varDecl.identifier & "' not found in scope",
          )

    # Process initializer
    if varDecl.initializer.isSome:
      analyzeTypeInference(inferencer, scope, varDecl.initializer.get())
  of NkAssignment:
    # For assignments, we need to infer the right side and possibly update the left variable's type
    let identifier = node.assignmentNode.identifier
    var valueType = inferExpressionType(inferencer, scope, node.assignmentNode.value)

    # Look up the variable being assigned to
    let symbolOpt = scope.findSymbol(identifier, node.pos)
    if symbolOpt.isSome and symbolOpt.get().kind == Variable:
      let symbol = symbolOpt.get()

      # Resolve inferred family types to concrete types if variable has MkToInfer
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
      # Check for incompatible types
      elif (isIntFamily(symbol.varType) and isFloatFamily(valueType)) or
          (isFloatFamily(symbol.varType) and isIntFamily(valueType)):
        inferencer.inferenceError(
          node.pos,
          "Type mismatch in assignment: cannot assign " & $valueType & " to " &
            $symbol.varType,
        )

    # Process the value expression
    analyzeTypeInference(inferencer, scope, node.assignmentNode.value)
  of NkFunDecl:
    # Process function body if present
    let funDecl = node.funDeclNode
    if funDecl.body.isSome:
      # Use function scope for analyzing the body
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeTypeInference(inferencer, functionScope, funDecl.body.get())
  of NkBlockStmt:
    # Process statements in block scope
    let blockScope = scope.children[node.blockStmtNode.blockId]
    for stmt in node.blockStmtNode.statements:
      analyzeTypeInference(inferencer, blockScope, stmt)
  of NkExprStmt:
    # Process expression statements
    analyzeTypeInference(inferencer, scope, node.exprStmtNode.expression)
  of NkReturnStmt:
    # Process return statements
    if node.returnStmtNode.expression.isSome:
      analyzeTypeInference(inferencer, scope, node.returnStmtNode.expression.get())
  of NkIfStmt:
    # Process both branches of the if statement
    for branch in node.ifStmtNode.branches:
      let branchScope = scope.children[branch.scopeId]
      analyzeTypeInference(inferencer, scope, branch.condition)
      analyzeTypeInference(inferencer, branchScope, branch.body)

    # Process else branch if present
    if node.ifStmtNode.elseBranch.isSome:
      let elseBranch = node.ifStmtNode.elseBranch.get()
      let elseScope = scope.children[elseBranch.scopeId]
      analyzeTypeInference(inferencer, elseScope, elseBranch.body)
  of NkLogicalExpr, NkEqualityExpr, NkComparisonExpr, NkAdditiveExpr,
      NkMultiplicativeExpr:
    # For these expressions, infer the type of the left and right operands
    analyzeTypeInference(inferencer, scope, node.binaryOpNode.left)
    analyzeTypeInference(inferencer, scope, node.binaryOpNode.right)
  of NkUnaryExpr:
    # For unary expressions, infer the type of the operand
    analyzeTypeInference(inferencer, scope, node.unaryOpNode.operand)
  of NkAddressOfExpr:
    # For address-of expressions, infer the type of the operand
    analyzeTypeInference(inferencer, scope, node.addressOfExprNode.operand)
  of NkDerefExpr:
    # For dereference expressions, infer the type of the operand
    analyzeTypeInference(inferencer, scope, node.derefExprNode.operand)
  of NkMemberAccess:
    # For member access, infer the type of the object
    analyzeTypeInference(inferencer, scope, node.memberAccessNode.obj)
  of NkGroupExpr:
    # For grouped expressions, infer the type of the inner expression
    analyzeTypeInference(inferencer, scope, node.groupNode.expression)
  of NkFunctionCall:
    # For function calls, infer the type of the callee and arguments
    analyzeTypeInference(inferencer, scope, node.functionCallNode.callee)
    for arg in node.functionCallNode.arguments:
      analyzeTypeInference(inferencer, scope, arg)

  # Literals already have known types, no inference needed
  of NkIntLiteral, NkIdentifier, NkUIntLiteral, NkFloatLiteral, NkStringLiteral,
      NkCStringLiteral, NkCharLiteral, NkBoolLiteral, NkNilLiteral, NkType, NkNop:
    discard
