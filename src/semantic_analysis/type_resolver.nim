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

proc resolveType*(
    resolver: TypeResolver, scope: Scope, typ: Type, pos: Position
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
      let resolvedTo = resolveType(resolver, scope, current.pointerTo, pos)
      if resolvedTo != current.pointerTo:
        # Only create a new pointer type if the target changed
        let oldHasAddress = current.hasAddress
        let oldFromRO = current.fromRO
        current = newPointerType(resolvedTo, current.kind == TkROPointer)
        current.hasAddress = oldHasAddress
        current.fromRO = oldFromRO
  # Recursively resolve struct member types
  elif current.kind == TkStruct:
    var changed = false
    let structType = current.structType
    for name, member in structType.members:
      let resolvedMemberType = resolveType(resolver, scope, member.typ, pos)
      if resolvedMemberType != member.typ:
        current.structType.members[name].typ = resolvedMemberType
        changed = true
    # No need to create a new struct type, just update in place
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

proc resolveExprType*(resolver: TypeResolver, scope: Scope, expr: Expr): Type =
  ## Recursively resolves the type of an expression and updates expr.exprType
  case expr.kind
  of EkIdentifier:
    let symbolOpt =
      scope.findSymbol(expr.identifierExpr.name, expr.pos, {Variable, Function})
    if symbolOpt.isSome:
      let symbol = symbolOpt.get()
      if symbol.kind == Variable:
        expr.exprType = resolveType(
          resolver, scope, symbol.declStmt.varDeclStmt.typeAnnotation, expr.pos
        )
        # Set addressability and RO flags
        expr.exprType.hasAddress = true
        expr.exprType.fromRO = symbol.declStmt.varDeclStmt.isReadOnly
        # If symbol type is meta, update declaration type on first use
        if symbol.declStmt.varDeclStmt.typeAnnotation.kind == TkMeta and
            symbol.declStmt.varDeclStmt.typeAnnotation.metaKind in
            {MkAnyInt, MkAnyUInt, MkAnyFloat}:
          symbol.declStmt.varDeclStmt.typeAnnotation = expr.exprType
      elif symbol.kind == Function:
        # functions do not have a type yet
        expr.exprType =
          Type(kind: TkMeta, metaKind: MkResolveError, name: "CannotResolveType")
      else:
        resolver.typeResolveError(expr.pos, "Unknown symbol kind: " & $symbol.kind)
    else:
      resolver.typeResolveError(
        expr.pos, "Unknown identifier: " & expr.identifierExpr.name
      )
  of EkIntLiteral, EkUIntLiteral, EkFloatLiteral, EkStringLiteral, EkCharLiteral,
      EkBoolLiteral, EkNilLiteral, EkCStringLiteral:
    # Literals already have meta types, nothing to do
    discard
  of EkAssignment:
    let rhsType = resolveExprType(resolver, scope, expr.assignmentExpr.value)
    let lhsType = resolveExprType(resolver, scope, expr.assignmentExpr.left)
    expr.exprType = new Type
    # Collapse meta type if needed (only for identifier left-hand side)
    var finalType =
      if expr.assignmentExpr.left.kind == EkIdentifier and lhsType.kind == TkMeta and
          lhsType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}: rhsType else: lhsType
    expr.exprType[] = finalType[]
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
          symbol.declStmt.varDeclStmt.typeAnnotation = finalType
      else:
        resolver.typeResolveError(
          expr.pos,
          "Assignment to unknown variable: " &
            expr.assignmentExpr.left.identifierExpr.name,
        )
  of EkLogicalExpr:
    let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
    let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
    # Only allow bool and bool, return bool
    if leftType.kind == TkPrimitive and rightType.kind == TkPrimitive and
        leftType.primitive == Bool and rightType.primitive == Bool:
      expr.exprType = leftType
    else:
      resolver.typeResolveError(
        expr.pos,
        "Logical operation requires boolean operands, got: " & $leftType & " and " &
          $rightType,
      )
      expr.exprType = leftType
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  of EkEqualityExpr:
    let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
    let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
    # Equality works over any types, but returns boolean
    expr.exprType = Type(kind: TkPrimitive, primitive: Bool)
  of EkComparisonExpr:
    let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
    let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
    # Allow numbers if:
    # - both are primitive and are the same type of primitive, return bool
    # - 1 is anyX and the other is X family, return bool
    # - both are the same type of any type, return bool
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
      expr.exprType = Type(kind: TkPrimitive, primitive: Bool)
    else:
      resolver.typeResolveError(
        expr.pos,
        "Comparison requires compatible numeric types, got: " & $leftType & " and " &
          $rightType,
      )
      expr.exprType = Type(kind: TkPrimitive, primitive: Bool)
  of EkAdditiveExpr:
    let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
    let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
    let op = $expr.binaryOpExpr.operator
    # Pointer arithmetic: pointer +/- anyint/anyuint family
    if (
      leftType.kind == TkPointer and (rightType.isIntFamily or rightType.isUIntFamily) and
      op in ["+", "-"]
    ):
      expr.exprType = leftType
    elif (
      leftType.kind == TkROPointer and (rightType.isIntFamily or rightType.isUIntFamily) and
      op in ["+", "-"]
    ):
      expr.exprType = leftType
    elif (
      rightType.kind == TkPointer and (leftType.isIntFamily or leftType.isUIntFamily) and
      op == "+"
    ):
      expr.exprType = rightType
    elif (
      rightType.kind == TkROPointer and (leftType.isIntFamily or leftType.isUIntFamily) and
      op == "+"
    ):
      expr.exprType = rightType
    # both are primitive and are the same type of primitive, return the primitive type
    elif leftType.kind == TkPrimitive and rightType.kind == TkPrimitive and
        leftType.primitive == rightType.primitive and
        leftType.primitive in {Int, UInt, Float}:
      expr.exprType = leftType
    # 1 is anyX and the other is X family, return the one that is X family
    elif leftType.kind == TkMeta and rightType.isIntFamily and
        leftType.metaKind == MkAnyInt:
      expr.exprType = rightType
    elif rightType.kind == TkMeta and leftType.isIntFamily and
        rightType.metaKind == MkAnyInt:
      expr.exprType = leftType
    elif leftType.kind == TkMeta and rightType.isUIntFamily and
        leftType.metaKind == MkAnyUInt:
      expr.exprType = rightType
    elif rightType.kind == TkMeta and leftType.isUIntFamily and
        rightType.metaKind == MkAnyUInt:
      expr.exprType = leftType
    elif leftType.kind == TkMeta and rightType.isFloatFamily and
        leftType.metaKind == MkAnyFloat:
      expr.exprType = rightType
    elif rightType.kind == TkMeta and leftType.isFloatFamily and
        rightType.metaKind == MkAnyFloat:
      expr.exprType = leftType
    # both are the same type of any type, return the anytype
    elif leftType.kind == TkMeta and rightType.kind == TkMeta and
        leftType.metaKind == rightType.metaKind and
        leftType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}:
      expr.exprType = leftType
    else:
      resolver.typeResolveError(
        expr.pos,
        "Type mismatch in additive operation: " & $leftType & " " & op & " " & $rightType,
      )
      expr.exprType = leftType
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  of EkMultiplicativeExpr:
    let leftType = resolveExprType(resolver, scope, expr.binaryOpExpr.left)
    let rightType = resolveExprType(resolver, scope, expr.binaryOpExpr.right)
    let op = $expr.binaryOpExpr.operator
    # both are primitive and are the same type of primitive, return the primitive type
    if leftType.kind == TkPrimitive and rightType.kind == TkPrimitive and
        leftType.primitive == rightType.primitive and
        leftType.primitive in {Int, UInt, Float}:
      expr.exprType = leftType
    # 1 is anyX and the other is X family, return the one that is X family
    elif leftType.kind == TkMeta and rightType.isIntFamily and
        leftType.metaKind == MkAnyInt:
      expr.exprType = rightType
    elif rightType.kind == TkMeta and leftType.isIntFamily and
        rightType.metaKind == MkAnyInt:
      expr.exprType = leftType
    elif leftType.kind == TkMeta and rightType.isUIntFamily and
        leftType.metaKind == MkAnyUInt:
      expr.exprType = rightType
    elif rightType.kind == TkMeta and leftType.isUIntFamily and
        rightType.metaKind == MkAnyUInt:
      expr.exprType = leftType
    elif leftType.kind == TkMeta and rightType.isFloatFamily and
        leftType.metaKind == MkAnyFloat:
      expr.exprType = rightType
    elif rightType.kind == TkMeta and leftType.isFloatFamily and
        rightType.metaKind == MkAnyFloat:
      expr.exprType = leftType
    # both are the same type of any type, return the anytype
    elif leftType.kind == TkMeta and rightType.kind == TkMeta and
        leftType.metaKind == rightType.metaKind and
        leftType.metaKind in {MkAnyInt, MkAnyUInt, MkAnyFloat}:
      expr.exprType = leftType
    else:
      resolver.typeResolveError(
        expr.pos,
        "Type mismatch in multiplicative operation: " & $leftType & " " & op & " " &
          $rightType,
      )
      expr.exprType = leftType
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  of EkUnaryExpr:
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
    expr.exprType = operandType
    # Unary ops: result is not addressable and not from RO
    expr.exprType.hasAddress = false
    expr.exprType.fromRO = false
  of EkMemberAccess:
    var objType = resolveExprType(resolver, scope, expr.memberAccessExpr.obj)
    # If objType is a pointer or const pointer to struct, dereference
    if objType.kind in {TkPointer, TkROPointer} and not objType.pointerTo.isNil and
        objType.pointerTo.kind == TkStruct:
      objType = objType.pointerTo
      objType.hasAddress = true
      objType.fromRO = objType.kind == TkROPointer
    if objType.kind == TkStruct:
      let structType = objType.structType
      if structType.members.hasKey(expr.memberAccessExpr.member):
        expr.exprType = new Type
        expr.exprType[] = structType.members[expr.memberAccessExpr.member].typ[]
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
  of EkFunctionCall:
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
        expr.exprType =
          resolveType(resolver, scope, symbol.declStmt.funDeclStmt.returnType, expr.pos)
      else:
        resolver.typeResolveError(
          expr.pos,
          "Call to unknown function: " & expr.functionCallExpr.callee.identifierExpr.name,
        )
    else:
      expr.exprType = calleeType
  of EkGroupExpr:
    expr.exprType = resolveExprType(resolver, scope, expr.groupExpr.expression)
  of EkAddressOfExpr:
    let operandType = resolveExprType(resolver, scope, expr.addressOfExpr.operand)
    # Only allow address-of if operand is addressable
    if not operandType.hasAddress:
      resolver.typeResolveError(
        expr.pos, "Cannot take address of non-addressable value"
      )
      # Still produce a pointer type for error recovery
      expr.exprType = newPointerType(operandType, false)
      expr.exprType.hasAddress = false
      expr.exprType.fromRO = false
    else:
      # Pointer type is RO if operand is fromRO
      expr.exprType = newPointerType(operandType, operandType.fromRO)
      expr.exprType.hasAddress = false
      expr.exprType.fromRO = false
  of EkDerefExpr:
    let operandType = resolveExprType(resolver, scope, expr.derefExpr.operand)
    if operandType.kind in {TkPointer, TkROPointer}:
      expr.exprType = operandType.pointerTo
      # Deref result is addressable if pointer is not RO
      expr.exprType.hasAddress = operandType.kind == TkPointer
      expr.exprType.fromRO = operandType.kind == TkROPointer
    else:
      resolver.typeResolveError(
        expr.pos, "Cannot dereference non-pointer type: " & $operandType
      )
      expr.exprType = operandType
  of EkStructLiteral:
    # Resolve struct type
    let symbolOpt = scope.findSymbol(expr.structLiteralExpr.typeName, expr.pos, Type)
    if symbolOpt.isSome:
      let symbol = symbolOpt.get()
      let structType = resolveType(resolver, scope, symbol.typeRepr, expr.pos)
      expr.exprType = structType
      expr.exprType.hasAddress = false
      expr.exprType.fromRO = false
    else:
      resolver.typeResolveError(
        expr.pos, "Unknown struct type: " & expr.structLiteralExpr.typeName
      )
  return expr.exprType

proc resolveStmtTypes*(resolver: TypeResolver, scope: Scope, stmt: Stmt) =
  ## Walks over the AST and resolves types for all expressions and updates symbols
  case stmt.kind
  of SkModule:
    for s in stmt.moduleStmt.statements:
      resolveStmtTypes(resolver, scope, s)
  of SkVarDecl:
    let varDecl = stmt.varDeclStmt
    stmt.varDeclStmt.typeAnnotation =
      resolveType(resolver, scope, varDecl.typeAnnotation, stmt.pos)
    if varDecl.initializer.isSome:
      let initType = resolveExprType(resolver, scope, varDecl.initializer.get)
      if varDecl.typeAnnotation.kind == TkMeta and
          varDecl.typeAnnotation.metaKind == MkToInfer:
        stmt.varDeclStmt.typeAnnotation = initType
  of SkFunDecl:
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
  of SkStructDecl:
    let structDecl = stmt.structDeclStmt
    for i, m in structDecl.members:
      structDecl.members[i].memberType =
        resolveType(resolver, scope, m.memberType, m.memberTypePos)
  of SkTypeDecl:
    let typeDecl = stmt.typeDeclStmt
    typeDecl.typeAnnotation =
      resolveType(resolver, scope, typeDecl.typeAnnotation, stmt.pos)
  of SkBlockStmt:
    let blockScope = scope.children[stmt.blockStmt.blockId]
    for s in stmt.blockStmt.statements:
      resolveStmtTypes(resolver, blockScope, s)
  of SkIfStmt:
    let ifStmt = stmt.ifStmt
    for branch in ifStmt.branches:
      discard resolveExprType(resolver, scope, branch.condition)
      let branchScope = scope.children[branch.scopeId]
      resolveStmtTypes(resolver, branchScope, branch.body)
    if ifStmt.elseBranch.isSome:
      let elseBranch = ifStmt.elseBranch.get
      let elseScope = scope.children[elseBranch.scopeId]
      resolveStmtTypes(resolver, elseScope, elseBranch.body)
  of SkWhileStmt:
    let whileScope = scope.children[stmt.whileStmt.scopeId]
    discard resolveExprType(resolver, scope, stmt.whileStmt.condition)
    if not whileScope.isNil:
      resolveStmtTypes(resolver, whileScope, stmt.whileStmt.body)
  of SkExprStmt:
    discard resolveExprType(resolver, scope, stmt.exprStmt.expression)
  of SkReturnStmt:
    if stmt.returnStmt.expression.isSome:
      discard resolveExprType(resolver, scope, stmt.returnStmt.expression.get)
  of SkNop:
    discard

func getBoolType(): Type =
  result = Type(kind: TkPrimitive)
  result.primitive = Bool
