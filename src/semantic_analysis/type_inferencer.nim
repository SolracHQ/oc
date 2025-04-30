import ../types/[file_info, position, scope, ast, types]
import ../reporter
import std/options
import std/tables

# Helper functions for type inference
proc isIntFamily*(typ: Type): bool =
  ## Returns true if the type is from the signed integer family
  (typ.kind == TkPrimitive and typ.primitive in {Int, Int8, Int16, Int32, Int64}) or
    (typ.kind == TkMeta and typ.metaKind == MkAnyInt)

proc isUIntFamily*(typ: Type): bool =
  ## Returns true if the type is from the unsigned integer family
  (typ.kind == TkPrimitive and typ.primitive in {UInt, UInt8, UInt16, UInt32, UInt64}) or
    (typ.kind == TkMeta and typ.metaKind == MkAnyUInt)

proc isFloatFamily*(typ: Type): bool =
  ## Returns true if the type is from the floating-point family
  (typ.kind == TkPrimitive and typ.primitive in {Float, Float32, Float64}) or
    (typ.kind == TkMeta and typ.metaKind == MkAnyFloat)

proc isConcreteType*(typ: Type): bool =
  ## Returns true if the type is concrete (not inferred)
  typ.kind == TkPrimitive or (
    typ.kind == TkMeta and
    typ.metaKind notin {MkAnyInt, MkAnyFloat, MkAnyUInt, MkNamedType}
  )

proc resolveInferredType*(typ: Type): Type =
  ## Resolves inferred types to their concrete counterparts
  case typ.kind
  of TkMeta:
    case typ.metaKind
    of MkAnyInt:
      result = Type(kind: TkPrimitive, primitive: Int)
    of MkAnyFloat:
      result = Type(kind: TkPrimitive, primitive: Float)
    of MkAnyUInt:
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
  logError("TypeInferencer", position, msg, hint)
  inferencer.hasError = true

proc newTypeInferencer*(fileInfo: FileInfo): TypeInferencer =
  ## Creates a new type inferencer for the given file
  result = TypeInferencer()
  result.fileInfo = fileInfo
  result.hasError = false

proc resolveType*(
    checker: TypeInferencer, scope: Scope, `type`: Type, pos: Position
): Type =
  ## Resolves a type to its actual representation
  var current = `type`
  var currentPos = pos
  # Resolve named types
  while current.kind == TkMeta and current.metaKind == MkNamedType:
    let symbolOpt = scope.findSymbol(current.name, currentPos, Type)
    if symbolOpt.isNone:
      checker.inferenceError(
        currentPos,
        "Unknown type '" & current.name & "'",
        "Declare the type before using it",
      )
      return current
    let symbol = symbolOpt.get()
    current = symbol.declStmt.typeDeclStmt.typeAnnotation
  # If struct, resolve all member types recursively
  if current.kind == TkStruct:
    for name, member in current.structType.members:
      let resolvedMemberType = checker.resolveType(scope, member.typ, pos)
      if member.typ != resolvedMemberType:
        current.structType.members[name].typ = resolvedMemberType
  return current

proc inferExpressionType*(inferencer: TypeInferencer, scope: Scope, expr: Expr): Type =
  ## Infers the type of an expression node
  return expr.exprType

proc analyzeTypeInference*(inferencer: TypeInferencer, scope: Scope, expr: Expr)

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
          of MkAnyInt:
            inferredType = Type(kind: TkPrimitive, primitive: Int)
          of MkAnyFloat:
            inferredType = Type(kind: TkPrimitive, primitive: Float)
          of MkAnyUInt:
            inferredType = Type(kind: TkPrimitive, primitive: UInt)
          else:
            discard
        let symbolOpt = scope.findSymbol(varDecl.identifier, stmt.pos, Variable)
        if symbolOpt.isSome:
          inferredType.hasAddress = true
          stmt.varDeclStmt.typeAnnotation = inferredType
        else:
          inferencer.inferenceError(
            stmt.pos,
            "Internal error: symbol '" & varDecl.identifier & "' not found in scope",
          )
  of SkTypeDecl:
    # No type inference needed for type declarations, but could check typeAnnotation
    discard
  of SkStructDecl:
    # No type inference needed for struct declarations, but could check member types
    discard
  of SkExprStmt:
    # Expression statements do not need type inference
    let expr = stmt.exprStmt.expression
    analyzeTypeInference(inferencer, scope, expr)
  of SkReturnStmt:
    discard
  of SkFunDecl:
    let funDecl = stmt.funDeclStmt
    if funDecl.body.isSome:
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeTypeInference(inferencer, functionScope, funDecl.body.get())
    var symbol = scope.findSymbol(funDecl.identifier, stmt.pos, Function)
    if symbol.isNone:
      inferencer.inferenceError(
        stmt.pos,
        "Internal error: function '" & funDecl.identifier & "' not found in scope",
      )
    else:
      for i, param in funDecl.parameters:
        if param.paramType.kind == TkMeta and param.paramType.metaKind == MkToInfer:
          funDecl.parameters[i].paramType =
            resolveType(inferencer, scope, param.paramType, stmt.pos)
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
  of SkWhileStmt:
    let whileStmt = stmt.whileStmt
    let whileScope = scope.children[whileStmt.scopeId]
    analyzeTypeInference(inferencer, whileScope, whileStmt.body)
  of SkNop:
    discard

# Expression analysis for assignment and other expressions
proc analyzeTypeInference*(inferencer: TypeInferencer, scope: Scope, expr: Expr) =
  case expr.kind
  of EkAssignment:
    let left = expr.assignmentExpr.left
    let value = expr.assignmentExpr.value
    var valueType = inferExpressionType(inferencer, scope, value)
    # If left is an identifier, handle type inference for variable
    if left.kind == EkIdentifier:
      let identifier = left.identifierExpr.name
      let symbolOpt = scope.findSymbol(identifier, expr.pos, Variable)
      if symbolOpt.isSome:
        let symbol = symbolOpt.get()
        let varDecl = symbol.declStmt.varDeclStmt
        if varDecl.typeAnnotation.kind == TkMeta and
            varDecl.typeAnnotation.metaKind == MkToInfer:
          if valueType.kind == TkMeta:
            case valueType.metaKind
            of MkAnyInt:
              valueType = Type(kind: TkPrimitive, primitive: Int)
            of MkAnyFloat:
              valueType = Type(kind: TkPrimitive, primitive: Float)
            of MkAnyUInt:
              valueType = Type(kind: TkPrimitive, primitive: UInt)
            else:
              discard
          varDecl.typeAnnotation = valueType
        elif varDecl.typeAnnotation.kind == TkMeta and
            varDecl.typeAnnotation.metaKind in {MkAnyInt, MkAnyFloat, MkAnyUInt}:
          # Int, Uint or Float by default
          if varDecl.typeAnnotation.metaKind == MkAnyInt:
            varDecl.typeAnnotation = Type(kind: TkPrimitive, primitive: Int)
          elif varDecl.typeAnnotation.metaKind == MkAnyFloat:
            varDecl.typeAnnotation = Type(kind: TkPrimitive, primitive: Float)
          elif varDecl.typeAnnotation.metaKind == MkAnyUInt:
            varDecl.typeAnnotation = Type(kind: TkPrimitive, primitive: UInt)
        elif (isIntFamily(varDecl.typeAnnotation) and isFloatFamily(valueType)) or
            (isFloatFamily(varDecl.typeAnnotation) and isIntFamily(valueType)):
          inferencer.inferenceError(
            expr.pos,
            "Type mismatch in assignment: cannot assign " & $valueType & " to " &
              $varDecl.typeAnnotation,
          )
    # Always analyze left and right expressions for type inference
    analyzeTypeInference(inferencer, scope, left)
    analyzeTypeInference(inferencer, scope, value)
  of EkIdentifier:
    # No type inference needed for identifiers
    discard
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr,
      EkMultiplicativeExpr:
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
  of EkStructLiteral:
    # Analyze all member expressions
    for member in expr.structLiteralExpr.members:
      analyzeTypeInference(inferencer, scope, member.value)
  of EkArrayAccess:
    # Analyze array and index expressions
    analyzeTypeInference(inferencer, scope, expr.arrayAccessExpr.arrayExpr)
    analyzeTypeInference(inferencer, scope, expr.arrayAccessExpr.indexExpr)
  of EkArrayLiteral:
    # Analyze all elements in the array literal
    for element in expr.arrayLiteralExpr.elements:
      analyzeTypeInference(inferencer, scope, element)
  # Literals and identifiers do not need further analysis
  of EkIntLiteral, EkUIntLiteral, EkFloatLiteral, EkStringLiteral, EkCStringLiteral,
      EkCharLiteral, EkBoolLiteral, EkNilLiteral:
    discard
