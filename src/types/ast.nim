import position
import types
import token
import annotation
import std/options
import std/tables

type
  StmtKind* = enum
    SkModule
    SkVarDecl
    SkFunDecl
    SkTypeDecl
    SkStructDecl
    SkBlockStmt
    SkExprStmt
    SkReturnStmt
    SkIfStmt
    SkWhileStmt
    SkNop

  ExprKind* = enum
    EkAssignment
    EkLogicalExpr
    EkEqualityExpr
    EkComparisonExpr
    EkAdditiveExpr
    EkMultiplicativeExpr
    EkUnaryExpr
    EkMemberAccess
    EkFunctionCall
    EkIdentifier
    EkGroupExpr
    EkAddressOfExpr
    EkDerefExpr
    EkIntLiteral
    EkUIntLiteral
    EkFloatLiteral
    EkStringLiteral
    EkCStringLiteral
    EkCharLiteral
    EkBoolLiteral
    EkNilLiteral
    EkStructLiteral

  # Program
  ModuleStmt* = object
    name*: string
    statements*: seq[Stmt]

  # Statements
  VarDeclStmt* = ref object
    isPublic*: bool
    isReadOnly*: bool # Indicates if this is a 'let' declaration
    identifier*: string
    typeAnnotation*: Type
    initializer*: Option[Expr]

  TypeDeclStmt* = ref object
    isPublic*: bool
    identifier*: string
    typeAnnotation*: Type

  StructMember* = object
    name*: string
    namePos*: Position
    memberType*: Type
    memberTypePos*: Position
    comments*: seq[string]
    annotations*: Annotations
    isPublic*: bool
    defaultValue*: Option[Expr]

  StructDeclStmt* = ref object
    isPublic*: bool
    identifier*: string
    identifierPos*: Position
    members*: Table[string, StructMember]

  FunctionParam* = object
    name*: string
    namePos*: Position
    paramType*: Type
    paramTypePos*: Position
    defaultValue*: Option[Expr]

  FunDeclStmt* = ref object
    identifier*: string
    identifierPos*: Position
    parameters*: seq[FunctionParam]
    returnType*: Type
    returnTypePos*: Position
    body*: Option[Stmt]
    isPublic*: bool

  BlockStmt* = object
    blockId*: string
    statements*: seq[Stmt]

  ExprStmt* = object
    expression*: Expr

  ReturnStmt* = object
    expression*: Option[Expr]

  IfStmt* = ref object
    branches*: seq[IfBranch]
    elseBranch*: Option[ElseBranch]

  IfBranch* = object
    scopeId*: string
    condition*: Expr
    body*: Stmt

  ElseBranch* = object
    scopeId*: string
    body*: Stmt

  WhileStmt* = ref object
    scopeId*: string
    condition*: Expr
    body*: Stmt

  # Statements
  Stmt* = ref object
    pos*: Position
    annotations*: Annotations
    comments*: seq[string]
    case kind*: StmtKind
    of SkModule: moduleStmt*: ModuleStmt
    of SkVarDecl: varDeclStmt*: VarDeclStmt
    of SkFunDecl: funDeclStmt*: FunDeclStmt
    of SkTypeDecl: typeDeclStmt*: TypeDeclStmt
    of SkStructDecl: structDeclStmt*: StructDeclStmt
    of SkBlockStmt: blockStmt*: BlockStmt
    of SkExprStmt: exprStmt*: ExprStmt
    of SkReturnStmt: returnStmt*: ReturnStmt
    of SkIfStmt: ifStmt*: IfStmt
    of SkWhileStmt: whileStmt*: WhileStmt
    of SkNop: discard

  # Expressions
  AssignmentExpr* = object
    left*: Expr
    value*: Expr

  BinaryOpExpr* = object
    left*: Expr
    operator*: TokenKind
    right*: Expr

  UnaryOpExpr* = object
    operator*: TokenKind
    operand*: Expr

  MemberAccessExpr* = object
    obj*: Expr
    member*: string

  FunctionCallExpr* = object
    callee*: Expr
    arguments*: seq[Expr]

  IdentifierExpr* = object
    name*: string

  GroupExpr* = object
    expression*: Expr

  AddressOfExpr* = object
    operand*: Expr

  DerefExpr* = object
    operand*: Expr

  StructLiteralMember* = object
    name*: string
    namePos*: Position
    value*: Expr

  StructLiteralExpr* = object
    typeName*: string
    members*: seq[StructLiteralMember]

  # Literals
  IntLiteralExpr* = object
    value*: int

  UIntLiteralExpr* = object
    value*: uint

  FloatLiteralExpr* = object
    value*: float

  StringLiteralExpr* = object
    value*: string

  CStringLiteralExpr* = object
    value*: string

  CharLiteralExpr* = object
    value*: char

  BoolLiteralExpr* = object
    value*: bool

  # Expressions
  Expr* = ref object
    pos*: Position
    annotations*: Annotations
    exprType*: Type
    case kind*: ExprKind
    of EkAssignment: assignmentExpr*: AssignmentExpr
    of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr,
        EkMultiplicativeExpr: binaryOpExpr*: BinaryOpExpr
    of EkUnaryExpr: unaryOpExpr*: UnaryOpExpr
    of EkMemberAccess: memberAccessExpr*: MemberAccessExpr
    of EkFunctionCall: functionCallExpr*: FunctionCallExpr
    of EkIdentifier: identifierExpr*: IdentifierExpr
    of EkGroupExpr: groupExpr*: GroupExpr
    of EkAddressOfExpr: addressOfExpr*: AddressOfExpr
    of EkDerefExpr: derefExpr*: DerefExpr
    of EkIntLiteral: intLiteralExpr*: IntLiteralExpr
    of EkUIntLiteral: uintLiteralExpr*: UIntLiteralExpr
    of EkFloatLiteral: floatLiteralExpr*: FloatLiteralExpr
    of EkStringLiteral: stringLiteralExpr*: StringLiteralExpr
    of EkCStringLiteral: cStringLiteralExpr*: CStringLiteralExpr
    of EkCharLiteral: charLiteralExpr*: CharLiteralExpr
    of EkBoolLiteral: boolLiteralExpr*: BoolLiteralExpr
    of EkNilLiteral: discard
    of EkStructLiteral: structLiteralExpr*: StructLiteralExpr
