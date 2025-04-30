import ../types/[ast, types, scope, position, token]
import ../reporter
import std/options
import std/sets
import std/tables

type TypeResolver* = ref object
  hasError*: bool

proc typeResolveError*(
    resolver: TypeResolver, pos: Position, msg: string, hint: string = ""
) =
  logError("TypeResolver", pos, msg, hint)
  resolver.hasError = true

type StringSet = ref HashSet[string]

proc resolveExprType*(resolver: TypeResolver, scope: Scope, expr: Expr): Type
proc resolveStmtTypes*(resolver: TypeResolver, scope: Scope, stmt: Stmt)

proc resolveType*(
    resolver: TypeResolver, scope: Scope, typ: Type, pos: Position, solvedStructs: StringSet = StringSet()
): Type =
  ## Resolves a type to its actual representation (resolves type aliases)
  var current = typ
  var currentPos = pos
  var seen = initHashSet[string]()
  # Resolve named types (type aliases)
  while current.kind == TkMeta and current.metaKind == MkNamedType:
    if seen.contains(current.name):
      resolver.typeResolveError(currentPos, "Cyclic type alias: " & current.name)
      return current
    seen.incl(current.name)
    let symbolOpt = scope.findSymbol(current.name, currentPos, Type)
    if symbolOpt.isNone:
      resolver.typeResolveError(
        currentPos,
        "Unknown type '" & current.name & "'",
        "Declare the type before using it",
      )
      return current
    let symbol = symbolOpt.get()
    current = symbol.typeRepr
  # Recursively resolve pointer types
  if current.kind == TkPointer or current.kind == TkROPointer:
    if not current.pointerTo.isNil:
      let resolvedTo = resolveType(resolver, scope, current.pointerTo, pos, solvedStructs)
      if resolvedTo != current.pointerTo:
        # Only create a new pointer type if the target changedW
        let oldHasAddress = current.hasAddress
        let oldFromRO = current.fromRO
        current = newPointerType(resolvedTo, current.kind == TkROPointer)
        current.hasAddress = oldHasAddress
        current.fromRO = oldFromRO
  elif current.kind == TkArray:
    # Resolve array types
    if current.arrayType.typ.kind == TkMeta and current.arrayType.typ.metaKind == MkNamedType:
      let symbolOpt = scope.findSymbol(current.arrayType.typ.name, pos, Type)
      if symbolOpt.isSome:
        let symbol = symbolOpt.get()
        current.arrayType.typ = resolveType(resolver, scope, symbol.typeRepr, pos, solvedStructs)
      else:
        resolver.typeResolveError(
          pos,
          "Unknown type '" & current.arrayType.typ.name & "'",
          "Declare the type before using it",
        )
  elif current.kind == TkStruct:
    # Resolve struct types
    if current.structType.name in solvedStructs[]:
      return current
    solvedStructs[].incl(current.structType.name)
    for i, m in current.structType.members:
      current.structType.members[i].typ = resolveType(resolver, scope, m.typ, pos, solvedStructs)
  # Preserve hasAddress and fromRO for all resolved types
  current.hasAddress = typ.hasAddress
  current.fromRO = typ.fromRO
  return current

proc collapseMetaType*(
    resolver: TypeResolver, left, right: Type, op: string, pos: Position
): Type =
  ## Collapses meta types (AnyInt, AnyUInt, AnyFloat) to a concrete type if possible
  # If both are meta, keep left; if one is concrete, prefer concrete
  if left.kind == TkMeta:
    case left.metaKind
    of MkAnyInt, MkAnyUInt, MkAnyFloat:
      if right.kind == TkPrimitive:
        return right
      elif right.kind == TkPointer and left.metaKind in {MkAnyInt, MkAnyUInt} and
          op in ["+", "-"]:
        return right
      elif right.kind == TkMeta and right.metaKind == left.metaKind:
        return left
      else:
        resolver.typeResolveError(
          pos, "Cannot collapse meta type " & $left & " with " & $right
        )
        return left
    else:
      return left
  return left

proc isIntFamily*(t: Type): bool =
  ## Checks if the type is an integer family type (Int, Int8, Int16, Int32, Int64, AnyInt)
  result =
    t.kind == TkPrimitive and t.primitive in {Int, Int8, Int16, Int32, Int64} or
    t.kind == TkMeta and t.metaKind in {MkAnyInt}

proc isFloatFamily*(t: Type): bool =
  ## Checks if the type is a float family type (Float, Float32, Float64, AnyFloat)
  result =
    t.kind == TkPrimitive and t.primitive in {Float, Float32, Float64} or
    t.kind == TkMeta and t.metaKind in {MkAnyFloat}

proc isUIntFamily*(t: Type): bool =
  ## Checks if the type is an unsigned integer family type (UInt, UInt8, UInt16, UInt32, UInt64, AnyUInt)
  result =
    t.kind == TkPrimitive and t.primitive in {UInt, UInt8, UInt16, UInt32, UInt64} or
    t.kind == TkMeta and t.metaKind in {MkAnyUInt}

proc isPointerFamily*(t: Type): bool =
  ## Checks if the type is a pointer family type (Pointer, ROPointer, AnyPointer)
  result =
    t.kind == TkPointer or t.kind == TkROPointer or
    (t.kind == TkMeta and t.metaKind == MkAnyPointer)

# --- Expr helpers ---

proc resolveIdentifierExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let symbolOpt =
    scope.findSymbol(expr.identifierExpr.name, expr.pos, {Variable, Function})
  if symbolOpt.isSome:
    let symbol = symbolOpt.get()
    if symbol.kind == Variable:
      let resolvedType = resolveType(
        resolver, scope, symbol.declStmt.varDeclStmt.typeAnnotation, expr.pos
      )
      expr.exprType = new Type
      expr.exprType[] = resolvedType[]
      # Set addressability and RO flags
      expr.exprType.hasAddress = true
      expr.exprType.fromRO = symbol.declStmt.varDeclStmt.isReadOnly
      # If symbol type is meta, update declaration type on first use
      if symbol.declStmt.varDeclStmt.typeAnnotation.kind == TkMeta and
          symbol.declStmt.varDeclStmt.typeAnnotation.metaKind in
          {MkAnyInt, MkAnyUInt, MkAnyFloat}:
        symbol.declStmt.varDeclStmt.typeAnnotation = expr.exprType
    elif symbol.kind == Function:
      expr.exprType = new Type
      expr.exprType[] = Type(kind: TkMeta, metaKind: MkResolveError, name: "CannotResolveType")[]
    else:
      resolver.typeResolveError(expr.pos, "Unknown symbol kind: " & $symbol.kind)
  else:
    resolver.typeResolveError(
      expr.pos, "Unknown identifier: " & expr.identifierExpr.name
    )
  return expr.exprType

proc resolveAssignmentExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let rhsType = resolveExprType(resolver, scope, expr.assignmentExpr.value)
  let lhsType = resolveExprType(resolver, scope, expr.assignmentExpr.left)
  # Collapse meta type if needed (only for identifier left-hand side)
  var finalType =
    if expr.assignmentExpr.left.kind == EkIdentifier and lhsType.kind == TkMeta and
        lhsType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}: rhsType else: lhsType
  let resolvedType = resolveType(resolver, scope, finalType, expr.pos)
  expr.exprType = new Type
  expr.exprType[] = resolvedType[]
  # Assignment result is not addressable
  expr.exprType.hasAddress = false
  expr.exprType.fromRO = false
  # Update declaration type if it was meta and left is identifier
  if expr.assignmentExpr.left.kind == EkIdentifier:
    let symbolOpt = scope.findSymbol(
      expr.assignmentExpr.left.identifierExpr.name, expr.pos, Variable
    )
    if symbolOpt.isSome:
      let symbol = symbolOpt.get()
      if symbol.declStmt.varDeclStmt.typeAnnotation.kind == TkMeta and
          symbol.declStmt.varDeclStmt.typeAnnotation.metaKind in
          {MkAnyInt, MkAnyUInt, MkAnyFloat}:
        symbol.declStmt.varDeclStmt.typeAnnotation = expr.exprType
    else:
      resolver.typeResolveError(
        expr.pos,
        "Assignment to unknown variable: " &
          expr.assignmentExpr.left.identifierExpr.name,
      )
  return expr.exprType

proc resolveLogicalExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
  let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
  # Only allow bool and bool, return bool
  if leftType.kind == TkPrimitive and rightType.kind == TkPrimitive and
      leftType.primitive == Bool and rightType.primitive == Bool:
    let resolvedType = resolveType(resolver, scope, leftType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  else:
    resolver.typeResolveError(
      expr.pos,
      "Logical operation requires boolean operands, got: " & $leftType & " and " &
        $rightType,
    )
    let resolvedType = resolveType(resolver, scope, leftType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  expr.exprType.hasAddress = false
  expr.exprType.fromRO = false
  return expr.exprType

proc resolveEqualityExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let boolType = Type(kind: TkPrimitive, primitive: Bool)
  discard resolveExprType(resolver, scope, expr.binaryOpExpr.left)
  discard resolveExprType(resolver, scope, expr.binaryOpExpr.right)
  let resolvedType = resolveType(resolver, scope, boolType, expr.pos)
  expr.exprType = new Type
  expr.exprType[] = resolvedType[]
  return expr.exprType

proc resolveComparisonExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
  let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
  let boolType = Type(kind: TkPrimitive, primitive: Bool)
  let resolvedType = resolveType(resolver, scope, boolType, expr.pos)
  if (
    leftType.kind == TkPrimitive and rightType.kind == TkPrimitive and
    leftType.primitive == rightType.primitive and
    leftType.primitive in {Int, UInt, Float}
  ) or (
    leftType.kind == TkMeta and rightType.isIntFamily and leftType.metaKind == MkAnyInt
  ) or (
    rightType.kind == TkMeta and leftType.isIntFamily and
    rightType.metaKind == MkAnyInt
  ) or (
    leftType.kind == TkMeta and rightType.isUIntFamily and
    leftType.metaKind == MkAnyUInt
  ) or (
    rightType.kind == TkMeta and leftType.isUIntFamily and
    rightType.metaKind == MkAnyUInt
  ) or (
    leftType.kind == TkMeta and rightType.isFloatFamily and
    leftType.metaKind == MkAnyFloat
  ) or (
    rightType.kind == TkMeta and leftType.isFloatFamily and
    rightType.metaKind == MkAnyFloat
  ) or (
    leftType.kind == TkMeta and rightType.kind == TkMeta and
    leftType.metaKind == rightType.metaKind and
    leftType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}
  ):
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  else:
    resolver.typeResolveError(
      expr.pos,
      "Comparison requires compatible numeric types, got: " & $leftType & " and " &
        $rightType,
    )
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  return expr.exprType

proc resolveAdditiveExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
  let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
  let op = $expr.binaryOpExpr.operator
  var resultType: Type = nil
  # Pointer arithmetic: pointer +/- anyint/anyuint family
  if (
    leftType.kind == TkPointer and (rightType.isIntFamily or rightType.isUIntFamily) and
    op in ["+", "-"]
  ):
    resultType = leftType
  elif (
    leftType.kind == TkROPointer and (rightType.isIntFamily or rightType.isUIntFamily) and
    op in ["+", "-"]
  ):
    resultType = leftType
  elif (
    rightType.kind == TkPointer and (leftType.isIntFamily or leftType.isUIntFamily) and
    op == "+"
  ):
    resultType = rightType
  elif (
    rightType.kind == TkROPointer and (leftType.isIntFamily or leftType.isUIntFamily) and
    op == "+"
  ):
    resultType = rightType
  # both are primitive and are the same type of primitive, return the primitive type
  elif leftType.kind == TkPrimitive and rightType.kind == TkPrimitive and
      leftType.primitive == rightType.primitive and
      leftType.primitive in {Int, UInt, Float}:
    resultType = leftType
  # 1 is anyX and the other is X family, return the one that is X family
  elif leftType.kind == TkMeta and rightType.isIntFamily and
      leftType.metaKind == MkAnyInt:
    resultType = rightType
  elif rightType.kind == TkMeta and leftType.isIntFamily and
      rightType.metaKind == MkAnyInt:
    resultType = leftType
  elif leftType.kind == TkMeta and rightType.isUIntFamily and
      leftType.metaKind == MkAnyUInt:
    resultType = rightType
  elif rightType.kind == TkMeta and leftType.isUIntFamily and
      rightType.metaKind == MkAnyUInt:
    resultType = leftType
  elif leftType.kind == TkMeta and rightType.isFloatFamily and
      leftType.metaKind == MkAnyFloat:
    resultType = rightType
  elif rightType.kind == TkMeta and leftType.isFloatFamily and
      rightType.metaKind == MkAnyFloat:
    resultType = leftType
  # both are the same type of any type, return the anytype
  elif leftType.kind == TkMeta and rightType.kind == TkMeta and
      leftType.metaKind == rightType.metaKind and
      leftType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}:
    resultType = leftType
  else:
    resolver.typeResolveError(
      expr.pos,
      "Type mismatch in additive operation: " & $leftType & " " & op & " " & $rightType,
    )
    resultType = leftType
  let resolvedType = resolveType(resolver, scope, resultType, expr.pos)
  expr.exprType = new Type
  expr.exprType[] = resolvedType[]
  expr.exprType.hasAddress = false
  expr.exprType.fromRO = false
  return expr.exprType

proc resolveMultiplicativeExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
  let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
  let op = $expr.binaryOpExpr.operator
  var resultType: Type = nil
  # both are primitive and are the same type of primitive, return the primitive type
  if leftType.kind == TkPrimitive and rightType.kind == TkPrimitive and
      leftType.primitive == rightType.primitive and
      leftType.primitive in {Int, UInt, Float}:
    resultType = leftType
  # 1 is anyX and the other is X family, return the one that is X family
  elif leftType.kind == TkMeta and rightType.isIntFamily and
      leftType.metaKind == MkAnyInt:
    resultType = rightType
  elif rightType.kind == TkMeta and leftType.isIntFamily and
      rightType.metaKind == MkAnyInt:
    resultType = leftType
  elif leftType.kind == TkMeta and rightType.isUIntFamily and
      leftType.metaKind == MkAnyUInt:
    resultType = rightType
  elif rightType.kind == TkMeta and leftType.isUIntFamily and
      rightType.metaKind == MkAnyUInt:
    resultType = leftType
  elif leftType.kind == TkMeta and rightType.isFloatFamily and
      leftType.metaKind == MkAnyFloat:
    resultType = rightType
  elif rightType.kind == TkMeta and leftType.isFloatFamily and
      rightType.metaKind == MkAnyFloat:
    resultType = leftType
  # both are the same type of any type, return the anytype
  elif leftType.kind == TkMeta and rightType.kind == TkMeta and
      leftType.metaKind == rightType.metaKind and
      leftType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}:
    resultType = leftType
  else:
    resolver.typeResolveError(
      expr.pos,
      "Type mismatch in multiplicative operation: " & $leftType & " " & op & " " &
        $rightType,
    )
    resultType = leftType
  let resolvedType = resolveType(resolver, scope, resultType, expr.pos)
  expr.exprType = new Type
  expr.exprType[] = resolvedType[]
  expr.exprType.hasAddress = false
  expr.exprType.fromRO = false
  return expr.exprType

proc resolveUnaryExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let operandType = resolveExprType(resolver, scope, expr.unaryOpExpr.operand)
  let op = $expr.unaryOpExpr.operator
  # - allows IntFamily
  if op == "-":
    if not operandType.isIntFamily and not operandType.isFloatFamily:
      resolver.typeResolveError(
        expr.pos, "Unary minus is only allowed for integer or float types"
      )
  # ! allow only booleans
  elif op == "!":
    if not (operandType.kind == TkPrimitive and operandType.primitive == Bool):
      resolver.typeResolveError(
        expr.pos, "Logical not is only allowed for boolean types"
      )
  # Disallow unary minus for AnyUInt (already covered by above)
  let resolvedType = resolveType(resolver, scope, operandType, expr.pos)
  expr.exprType = new Type
  expr.exprType[] = resolvedType[]
  # Unary ops: result is not addressable and not from RO
  expr.exprType.hasAddress = false
  expr.exprType.fromRO = false
  return expr.exprType

proc resolveMemberAccessExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  var objType = resolveExprType(resolver, scope, expr.memberAccessExpr.obj)
  # If objType is a pointer or const pointer to struct, dereference
  if objType.kind == TkMeta and objType.metaKind == MkNamedType:
    # Resolve named type
    let symbolOpt = scope.findSymbol(objType.name, expr.pos, Type)
    if symbolOpt.isSome:
      let symbol = symbolOpt.get()
      objType = resolveType(resolver, scope, symbol.typeRepr, expr.pos)
    else:
      resolver.typeResolveError(
        expr.pos, "Unknown type: " & objType.name
      )
  if objType.kind in {TkPointer, TkROPointer} and not objType.pointerTo.isNil and
      (objType.pointerTo.kind == TkStruct or objType.pointerTo.kind == TkMeta and objType.pointerTo.metaKind == MkNamedType):
    objType = resolveType(
      resolver, scope, objType.pointerTo, expr.memberAccessExpr.obj.pos
    )
    objType.hasAddress = true
    objType.fromRO = objType.kind == TkROPointer
  if objType.kind == TkStruct:
    let structType = objType.structType
    if structType.members.hasKey(expr.memberAccessExpr.member):
      let memberType = structType.members[expr.memberAccessExpr.member].typ
      let resolvedType = resolveType(resolver, scope, memberType, expr.pos)
      expr.exprType = new Type
      expr.exprType[] = resolvedType[]
      # Propagate addressability and RO from the object
      expr.exprType.hasAddress = objType.hasAddress
      expr.exprType.fromRO = objType.fromRO
    else:
      resolver.typeResolveError(
        expr.pos, "Struct has no member: " & expr.memberAccessExpr.member
      )
  else:
    resolver.typeResolveError(
      expr.pos, "Member access on non-struct type: " & $objType
    )
  return expr.exprType

proc resolveFunctionCallExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let calleeType = resolveExprType(resolver, scope, expr.functionCallExpr.callee)
  # Assume callee is identifier, get symbol
  if expr.functionCallExpr.callee.kind == EkIdentifier:
    let symbolOpt = scope.findSymbol(
      expr.functionCallExpr.callee.identifierExpr.name, expr.pos, Function
    )
    if symbolOpt.isSome:
      let symbol = symbolOpt.get()
      # Resolve parameter types
      for i, arg in expr.functionCallExpr.arguments:
        let argType = resolveExprType(resolver, scope, arg)
        if i < symbol.declStmt.funDeclStmt.parameters.len:
          let paramType = resolveType(
            resolver,
            scope,
            symbol.declStmt.funDeclStmt.parameters[i].paramType,
            arg.pos,
          )
          # Collapse meta type if needed
          if paramType.kind == TkMeta and
              paramType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}:
            symbol.declStmt.funDeclStmt.parameters[i].paramType = argType
        # else: too many args, error
      let resolvedType = resolveType(resolver, scope, symbol.declStmt.funDeclStmt.returnType, expr.pos)
      expr.exprType = new Type
      expr.exprType[] = resolvedType[]
    else:
      resolver.typeResolveError(
        expr.pos,
        "Call to unknown function: " & expr.functionCallExpr.callee.identifierExpr.name,
      )
  else:
    let resolvedType = resolveType(resolver, scope, calleeType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  return expr.exprType

proc resolveGroupExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let innerType = resolveExprType(resolver, scope, expr.groupExpr.expression)
  let resolvedType = resolveType(resolver, scope, innerType, expr.pos)
  expr.exprType = new Type
  expr.exprType[] = resolvedType[]
  return expr.exprType

proc resolveAddressOfExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let operandType = resolveExprType(resolver, scope, expr.addressOfExpr.operand)
  # Only allow address-of if operand is addressable
  if not operandType.hasAddress:
    resolver.typeResolveError(
      expr.pos, "Cannot take address of non-addressable value"
    )
    # Still produce a pointer type for error recovery
    let ptrType = newPointerType(operandType, false)
    let resolvedType = resolveType(resolver, scope, ptrType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  else:
    # Pointer type is RO if operand is fromRO
    let ptrType = newPointerType(operandType, operandType.fromRO)
    let resolvedType = resolveType(resolver, scope, ptrType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  return expr.exprType

proc resolveDerefExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let operandType = resolveExprType(resolver, scope, expr.derefExpr.operand)
  if operandType.kind in {TkPointer, TkROPointer}:
    let resolvedType = resolveType(resolver, scope, operandType.pointerTo, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
    # Deref result is addressable if pointer is not RO
    expr.exprType.hasAddress = operandType.kind == TkPointer
    expr.exprType.fromRO = operandType.kind == TkROPointer
  else:
    resolver.typeResolveError(
      expr.pos, "Cannot dereference non-pointer type: " & $operandType
    )
    let resolvedType = resolveType(resolver, scope, operandType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  return expr.exprType

proc resolveStructLiteralExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  # Resolve struct type
  let symbolOpt = scope.findSymbol(expr.structLiteralExpr.typeName, expr.pos, Type)
  if symbolOpt.isSome:
    let symbol = symbolOpt.get()
    let structType = resolveType(resolver, scope, symbol.typeRepr, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = structType[]
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  else:
    resolver.typeResolveError(
      expr.pos, "Unknown struct type: " & expr.structLiteralExpr.typeName
    )
  return expr.exprType

proc resolveArrayAccessExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  let arrType = resolveExprType(resolver, scope, expr.arrayAccessExpr.arrayExpr)
  let idxType = resolveExprType(resolver, scope, expr.arrayAccessExpr.indexExpr)
  # Allow array, pointer, or CString
  if arrType.kind == TkArray:
    let resolvedType = resolveType(
      resolver, scope, arrType.arrayType.typ, expr.arrayAccessExpr.arrayExpr.pos
    )
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
    # Array element is addressable if array is addressable
    expr.exprType.hasAddress = arrType.hasAddress
    expr.exprType.fromRO = arrType.fromRO
  elif arrType.kind == TkPointer or arrType.kind == TkROPointer:
    if arrType.pointerTo.isNil:
      resolver.typeResolveError(expr.pos, "Pointer type has no target type")
      let resolvedType = resolveType(resolver, scope, arrType, expr.pos)
      expr.exprType = new Type
      expr.exprType[] = resolvedType[]
    else:
      let resolvedType = resolveType(resolver, scope, arrType.pointerTo, expr.pos)
      expr.exprType = new Type
      expr.exprType[] = resolvedType[]
      expr.exprType.hasAddress = arrType.kind == TkPointer
      expr.exprType.fromRO = arrType.kind == TkROPointer
  elif arrType.kind == TkPrimitive and arrType.primitive == CString:
    # CString: treat as pointer to char
    let charType = Type(kind: TkPrimitive, primitive: Char)
    let resolvedType = resolveType(resolver, scope, charType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  else:
    resolver.typeResolveError(
      expr.pos,
      "Array access requires array, pointer, or CString type, got: " & $arrType
    )
    let resolvedType = resolveType(resolver, scope, arrType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  # Index must be int or uint family
  if not (idxType.isIntFamily or idxType.isUIntFamily):
    resolver.typeResolveError(
      expr.arrayAccessExpr.indexExpr.pos,
      "Array index must be integer or unsigned integer type, got: " & $idxType
    )
  return expr.exprType

proc resolveArrayLiteralExpr(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  # If exprType is already set (from annotation), use it; else infer from elements
  if not expr.exprType.isNil and expr.exprType.kind == TkArray:
    let elemType = expr.exprType.arrayType.typ
    for el in expr.arrayLiteralExpr.elements:
      let elType = resolveExprType(resolver, scope, el)
      # Optionally: check compatibility here
    # exprType already set
    let resolvedType = resolveType(resolver, scope, expr.exprType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
  else:
    # Infer type from elements
    var inferredType: Type = nil
    let elements = expr.arrayLiteralExpr.elements
    for el in elements:
      let elType = resolveExprType(resolver, scope, el)
      if inferredType.isNil and elType.kind != TkMeta:
        inferredType = elType
    if inferredType.isNil:
      # All elements are meta or empty, fallback to AnyInt
      inferredType = Type(kind: TkMeta, metaKind: MkAnyInt)
    let arrType = Type(
      kind: TkArray,
      arrayType: ArrayType(typ: inferredType, size: elements.len)
    )
    let resolvedType = resolveType(resolver, scope, arrType, expr.pos)
    expr.exprType = new Type
    expr.exprType[] = resolvedType[]
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  return expr.exprType

proc resolveExprType*(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  case expr.kind
  of EkIdentifier:
    result = resolveIdentifierExpr(resolver, scope, expr)
  of EkIntLiteral, EkUIntLiteral, EkFloatLiteral, EkStringLiteral, EkCharLiteral,
      EkBoolLiteral, EkNilLiteral, EkCStringLiteral:
    result = expr.exprType
  of EkAssignment:
    result = resolveAssignmentExpr(resolver, scope, expr)
  of EkLogicalExpr:
    result = resolveLogicalExpr(resolver, scope, expr)
  of EkEqualityExpr:
    result = resolveEqualityExpr(resolver, scope, expr)
  of EkComparisonExpr:
    result = resolveComparisonExpr(resolver, scope, expr)
  of EkAdditiveExpr:
    result = resolveAdditiveExpr(resolver, scope, expr)
  of EkMultiplicativeExpr:
    result = resolveMultiplicativeExpr(resolver, scope, expr)
  of EkUnaryExpr:
    result = resolveUnaryExpr(resolver, scope, expr)
  of EkMemberAccess:
    result = resolveMemberAccessExpr(resolver, scope, expr)
  of EkFunctionCall:
    result = resolveFunctionCallExpr(resolver, scope, expr)
  of EkGroupExpr:
    result = resolveGroupExpr(resolver, scope, expr)
  of EkAddressOfExpr:
    result = resolveAddressOfExpr(resolver, scope, expr)
  of EkDerefExpr:
    result = resolveDerefExpr(resolver, scope, expr)
  of EkStructLiteral:
    result = resolveStructLiteralExpr(resolver, scope, expr)
  of EkArrayAccess:
    result = resolveArrayAccessExpr(resolver, scope, expr)
  of EkArrayLiteral:
    result = resolveArrayLiteralExpr(resolver, scope, expr)
  expr.exprType = new Type
  expr.exprType[] = result[]
  
# --- Stmt helpers ---

proc resolveModuleStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  for s in stmt.moduleStmt.statements:
    resolveStmtTypes(resolver, scope, s)

proc resolveVarDeclStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  let varDecl = stmt.varDeclStmt
  stmt.varDeclStmt.typeAnnotation =
    resolveType(resolver, scope, varDecl.typeAnnotation, stmt.pos)
  if varDecl.initializer.isSome:
    let initType = resolveExprType(resolver, scope, varDecl.initializer.get)
    if varDecl.typeAnnotation.kind == TkMeta and
        varDecl.typeAnnotation.metaKind == MkToInfer:
      stmt.varDeclStmt.typeAnnotation = initType

proc resolveFunDeclStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  let funDecl = stmt.funDeclStmt
  # Resolve parameter types
  for i, param in funDecl.parameters:
    funDecl.parameters[i].paramType =
      resolveType(resolver, scope, param.paramType, param.namePos)
  funDecl.returnType =
    resolveType(resolver, scope, funDecl.returnType, funDecl.returnTypePos)
  if funDecl.body.isSome:
    let funScope = scope.children[funDecl.identifier]
    resolveStmtTypes(resolver, funScope, funDecl.body.get)

proc resolveStructDeclStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  let structDecl = stmt.structDeclStmt
  for i, m in structDecl.members:
    structDecl.members[i].memberType =
      resolveType(resolver, scope, m.memberType, m.memberTypePos)

proc resolveTypeDeclStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  let typeDecl = stmt.typeDeclStmt
  typeDecl.typeAnnotation =
    resolveType(resolver, scope, typeDecl.typeAnnotation, stmt.pos)

proc resolveBlockStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  let blockScope = scope.children[stmt.blockStmt.blockId]
  for s in stmt.blockStmt.statements:
    resolveStmtTypes(resolver, blockScope, s)

proc resolveIfStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  let ifStmt = stmt.ifStmt
  for branch in ifStmt.branches:
    discard resolveExprType(resolver, scope, branch.condition)
    let branchScope = scope.children[branch.scopeId]
    resolveStmtTypes(resolver, branchScope, branch.body)
  if ifStmt.elseBranch.isSome:
    let elseBranch = ifStmt.elseBranch.get
    let elseScope = scope.children[elseBranch.scopeId]
    resolveStmtTypes(resolver, elseScope, elseBranch.body)

proc resolveWhileStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  let whileScope = scope.children[stmt.whileStmt.scopeId]
  discard resolveExprType(resolver, scope, stmt.whileStmt.condition)
  if not whileScope.isNil:
    resolveStmtTypes(resolver, whileScope, stmt.whileStmt.body)

proc resolveExprStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  discard resolveExprType(resolver, scope, stmt.exprStmt.expression)

proc resolveReturnStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  if stmt.returnStmt.expression.isSome:
    discard resolveExprType(resolver, scope, stmt.returnStmt.expression.get)

proc resolveNopStmt(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  discard

# --- Main stmt dispatcher ---

proc resolveStmtTypes*(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  case stmt.kind
  of SkModule:
    resolveModuleStmt(resolver, scope, stmt)
  of SkVarDecl:
    resolveVarDeclStmt(resolver, scope, stmt)
  of SkFunDecl:
    resolveFunDeclStmt(resolver, scope, stmt)
  of SkStructDecl:
    resolveStructDeclStmt(resolver, scope, stmt)
  of SkTypeDecl:
    resolveTypeDeclStmt(resolver, scope, stmt)
  of SkBlockStmt:
    resolveBlockStmt(resolver, scope, stmt)
  of SkIfStmt:
    resolveIfStmt(resolver, scope, stmt)
  of SkWhileStmt:
    resolveWhileStmt(resolver, scope, stmt)
  of SkExprStmt:
    resolveExprStmt(resolver, scope, stmt)
  of SkReturnStmt:
    resolveReturnStmt(resolver, scope, stmt)
  of SkNop:
    resolveNopStmt(resolver, scope, stmt)

func getBoolType(): Type =
  result = Type(kind: TkPrimitive)
  result.primitive = Bool
