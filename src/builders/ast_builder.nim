import ../types/[ast, position, types, token, annotation]
import std/options
import std/tables

proc newLiteralExpr*[T](value: T, position: Position): Expr =
  ## newLiteralExpr node with the given value and position
  when T is int:
    result = Expr(kind: EkIntLiteral, pos: position)
    result.intLiteralExpr.value = value
    result.exprType = Type(kind: TkMeta, metaKind: MkAnyInt)
  elif T is uint:
    result = Expr(kind: EkUIntLiteral, pos: position)
    result.uintLiteralExpr.value = value
    result.exprType = Type(kind: TkMeta, metaKind: MkAnyUInt)
  elif T is float:
    result = Expr(kind: EkFloatLiteral, pos: position)
    result.floatLiteralExpr.value = value
    result.exprType = Type(kind: TkMeta, metaKind: MkAnyFloat)
  elif T is string:
    result = Expr(kind: EkStringLiteral, pos: position)
    result.stringLiteralExpr.value = value
    result.exprType = Type(kind: TkPrimitive, primitive: String)
  elif T is char:
    result = Expr(kind: EkCharLiteral, pos: position)
    result.charLiteralExpr.value = value
    result.exprType = Type(kind: TkPrimitive, primitive: Char)
  elif T is bool:
    result = Expr(kind: EkBoolLiteral, pos: position)
    result.boolLiteralExpr.value = value
    result.exprType = Type(kind: TkPrimitive, primitive: Bool)
  else:
    {.error: "Unsupported literal type".}

proc newCStringLiteralExpr*(value: string, position: Position): Expr =
  ## Creates a C string literal expr with the given value and position
  result = Expr(kind: EkCStringLiteral, pos: position)
  result.cStringLiteralExpr.value = value
  result.exprType = Type(kind: TkPrimitive, primitive: CString)

proc newNilLiteralExpr*(position: Position): Expr =
  ## Creates a nil literal expr with the given position
  result = Expr(kind: EkNilLiteral, pos: position)
  result.exprType = Type(kind: TkMeta, metaKind: MkAnyPointer)

proc newIdentifierExpr*(name: string, position: Position): Expr =
  ## Creates an identifier expr with the given name and position
  result = Expr(kind: EkIdentifier, pos: position)
  result.identifierExpr.name = name
  result.exprType = Type(kind: TkMeta, metaKind: MkToInfer)

proc newGroupExpr*(expr: Expr, position: Position): Expr =
  ## Creates a group expression expr with the given expr and position
  result = Expr(kind: EkGroupExpr, pos: position)
  result.groupExpr.expression = expr
  result.exprType = expr.exprType

proc newAssignmentExpr*(left: Expr, value: Expr, position: Position): Expr =
  ## Creates an assignment expression
  result = Expr(kind: EkAssignment, pos: position)
  result.assignmentExpr = AssignmentExpr(left: left, value: value)
  result.exprType = value.exprType

proc newBinaryOpExpr*(
    kind: ExprKind, left: Expr, operator: TokenKind, right: Expr, position: Position
): Expr =
  ## Creates a binary operation expression (logical, equality, comparison, additive, multiplicative)
  result = Expr(kind: kind, pos: position)
  result.binaryOpExpr = BinaryOpExpr(left: left, operator: operator, right: right)

proc newUnaryOpExpr*(operator: TokenKind, operand: Expr, position: Position): Expr =
  ## Creates a unary operation expression
  result = Expr(kind: EkUnaryExpr, pos: position)
  result.unaryOpExpr = UnaryOpExpr(operator: operator, operand: operand)

proc newMemberAccessExpr*(obj: Expr, member: string, position: Position): Expr =
  ## Creates a member access expression
  result = Expr(kind: EkMemberAccess, pos: position)
  result.memberAccessExpr = MemberAccessExpr(obj: obj, member: member)

proc newFunctionCallExpr*(
    callee: Expr, arguments: seq[Expr], position: Position
): Expr =
  ## Creates a function call expression
  result = Expr(kind: EkFunctionCall, pos: position)
  result.functionCallExpr = FunctionCallExpr(callee: callee, arguments: arguments)

proc newAddressOfExpr*(operand: Expr, position: Position): Expr =
  ## Creates an address-of expression
  result = Expr(kind: EkAddressOfExpr, pos: position)
  result.addressOfExpr = AddressOfExpr(operand: operand)

proc newDerefExpr*(operand: Expr, position: Position): Expr =
  ## Creates a dereference expression
  result = Expr(kind: EkDerefExpr, pos: position)
  result.derefExpr = DerefExpr(operand: operand)

# --- Struct Literal Expression Constructor ---
proc newStructLiteralExpr*(
    typeName: string, members: seq[StructLiteralMember], position: Position
): Expr =
  ## Creates a struct literal expression
  result = Expr(kind: EkStructLiteral, pos: position)
  result.structLiteralExpr = StructLiteralExpr(typeName: typeName, members: members)

# --- Struct Literal Member Constructor ---
proc newStructLiteralMember*(
    name: string, namePos: Position, value: Expr
): StructLiteralMember =
  ## Creates a struct literal member
  StructLiteralMember(name: name, namePos: namePos, value: value)

proc newArrayAccessExpr*(arrayExpr: Expr, indexExpr: Expr, position: Position): Expr =
  ## Creates an array access expression
  result = Expr(kind: EkArrayAccess, pos: position)
  result.arrayAccessExpr = ArrayAccessExpr(arrayExpr: arrayExpr, indexExpr: indexExpr)

proc newArrayLiteralExpr*(elements: seq[Expr], position: Position): Expr =
  ## Creates an array literal expression
  result = Expr(kind: EkArrayLiteral, pos: position)
  result.arrayLiteralExpr = ArrayLiteralExpr(elements: elements)

# --- Statement Constructors ---

proc newVarDeclStmt*(
    identifier: string,
    typeAnnotation: Type,
    initializer: Option[Expr],
    isPublic: bool,
    isReadOnly: bool,
    pos: Position,
): Stmt =
  ## Creates a variable declaration statement
  result = Stmt(kind: SkVarDecl, pos: pos)
  result.varDeclStmt = VarDeclStmt(
    isPublic: isPublic,
    isReadOnly: isReadOnly,
    identifier: identifier,
    typeAnnotation: typeAnnotation,
    initializer: initializer,
  )

proc newExprStmt*(expr: Expr, pos: Position): Stmt =
  ## Creates an expression statement
  result = Stmt(kind: SkExprStmt, pos: pos)
  result.exprStmt = ExprStmt(expression: expr)

proc newReturnStmt*(expr: Option[Expr], pos: Position): Stmt =
  ## Creates a return statement
  result = Stmt(kind: SkReturnStmt, pos: pos)
  result.returnStmt = ReturnStmt(expression: expr)

proc newBlockStmt*(blockId: string, statements: seq[Stmt], pos: Position): Stmt =
  ## Creates a block statement
  result = Stmt(kind: SkBlockStmt, pos: pos)
  result.blockStmt = BlockStmt(blockId: blockId, statements: statements)

proc newWhileStmt*(scopeId: string, condition: Expr, body: Stmt, pos: Position): Stmt =
  ## Creates a while statement
  result = Stmt(kind: SkWhileStmt, pos: pos)
  result.whileStmt = WhileStmt(condition: condition, body: body, scopeId: scopeId)

# --- Struct Declaration Statement Constructor ---
proc newStructDeclStmt*(
    isPublic: bool,
    identifier: string,
    identifierPos: Position,
    members: Table[string, ast.StructMember],
    pos: Position,
): Stmt =
  ## Creates a struct declaration statement
  result = Stmt(kind: SkStructDecl, pos: pos)
  result.structDeclStmt = StructDeclStmt(
    isPublic: isPublic,
    identifier: identifier,
    identifierPos: identifierPos,
    members: members,
  )

# --- Struct Member Constructor ---
proc newStructMember*(
    name: string,
    namePos: Position,
    memberType: Type,
    memberTypePos: Position,
    comments: seq[string],
    annotations: Annotations,
    isPublic: bool,
    defaultValue: Option[Expr],
): ast.StructMember =
  ## Creates a struct member
  ast.StructMember(
    name: name,
    namePos: namePos,
    memberType: memberType,
    memberTypePos: memberTypePos,
    comments: comments,
    annotations: annotations,
    isPublic: isPublic,
    defaultValue: defaultValue,
  )

# --- Type Declaration Statement Constructor ---
proc newTypeDeclStmt*(
    isPublic: bool, identifier: string, typeAnnotation: Type, pos: Position
): Stmt =
  ## Creates a type declaration statement
  result = Stmt(kind: SkTypeDecl, pos: pos)
  result.typeDeclStmt = TypeDeclStmt(
    isPublic: isPublic, identifier: identifier, typeAnnotation: typeAnnotation
  )

# --- If Statement Builder ---
type IfStmtBuilder* = ref object
  branches*: seq[IfBranch]
  elseBranch*: Option[ElseBranch]
  pos*: Position

proc newIfStmtBuilder*(pos: Position): IfStmtBuilder =
  IfStmtBuilder(branches: @[], elseBranch: none(ElseBranch), pos: pos)

proc addIfBranch*(
    builder: IfStmtBuilder, scopeId: string, condition: Expr, body: Stmt
) =
  builder.branches.add IfBranch(scopeId: scopeId, condition: condition, body: body)

proc setElseBranch*(builder: IfStmtBuilder, scopeId: string, body: Stmt) =
  builder.elseBranch = some(ElseBranch(scopeId: scopeId, body: body))

proc buildIfStmt*(builder: IfStmtBuilder): Stmt =
  result = Stmt(kind: SkIfStmt, pos: builder.pos)
  result.ifStmt = IfStmt(branches: builder.branches, elseBranch: builder.elseBranch)

# --- Function Declaration Builder ---
type FunDeclBuilder* = ref object
  identifier*: string
  identifierPos*: Position
  parameters*: seq[FunctionParam]
  returnType*: Type
  returnTypePos*: Position
  body*: Option[Stmt]
  isPublic*: bool
  pos*: Position

proc newFunDeclBuilder*(
    identifier: string,
    identifierPos: Position,
    returnType: Type,
    returnTypePos: Position,
    isPublic: bool,
    pos: Position,
): FunDeclBuilder =
  FunDeclBuilder(
    identifier: identifier,
    identifierPos: identifierPos,
    parameters: @[],
    returnType: returnType,
    returnTypePos: returnTypePos,
    body: none(Stmt),
    isPublic: isPublic,
    pos: pos,
  )

proc addParameter*(builder: FunDeclBuilder, param: FunctionParam) =
  builder.parameters.add param

proc setBody*(builder: FunDeclBuilder, body: Stmt) =
  builder.body = some(body)

proc buildFunDeclStmt*(builder: FunDeclBuilder): Stmt =
  result = Stmt(kind: SkFunDecl, pos: builder.pos)
  result.funDeclStmt = FunDeclStmt(
    identifier: builder.identifier,
    identifierPos: builder.identifierPos,
    parameters: builder.parameters,
    returnType: builder.returnType,
    returnTypePos: builder.returnTypePos,
    body: builder.body,
    isPublic: builder.isPublic,
  )

proc newNopStmt*(pos: Position): Stmt =
  ## Creates a no-operation statement
  result = Stmt(kind: SkNop, pos: pos)
