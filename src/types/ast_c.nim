import std/options
import std/sets
import position
import types_c
import token

type
  CStmtKind* = enum
    CskTranslationUnit
    CskNop
    CskFunctionDecl
    CskFunctionDef
    CskVarDecl
    CskDefine
    CskInclude
    CskIfDef
    CskIfNotDef
    CskBlockStmt
    CskExprStmt
    CskReturnStmt
    CskIfStmt
    CskWhileStmt
    CskTypedef
    CskStructDef

  CExprKind* = enum
    CekAssignment
    CekBinaryExpr
    CekUnaryExpr
    CekFunctionCall
    CekIdentifier
    CekArrayAccess
    CekGroupExpr
    CekAddressOf
    CekDereference
    CekIntLiteral
    CekUIntLiteral
    CekFloatLiteral
    CekStringLiteral
    CekCharLiteral
    CekBoolLiteral
    CekNullLiteral
    CekStructLiteral
    CekMemberAccess
    CekArrowMemberAccess
    CekArrayLiteral

  # Program structure
  TranslationUnitNode* = object
    includes*: HashSet[IncludeNode]
    declarations*: seq[CStmt]

  # Declarations
  FunctionDeclNode* = object
    name*: string
    returnType*: CType
    parameters*: seq[ParameterNode]
    isStatic*: bool
    isExtern*: bool
    isInline*: bool

  FunctionDefNode* = object
    declaration*: CStmt # Must be CskFunctionDecl
    body*: CStmt # Must be CskBlockStmt

  ParameterNode* = object
    name*: string
    paramType*: CType

  VarDeclNode* = object
    name*: string
    varType*: CType
    initializer*: Option[CExpr]
    isConst*: bool
    isStatic*: bool
    isExtern*: bool
    isVolatile*: bool

  # Typedef
  TypedefNode* = object
    name*: string
    baseType*: CType

  # Struct definition
  StructDefNode* = object
    name*: string
    members*: seq[ParameterNode] # Use ParameterNode for member name/type

  # Preprocessor
  DefineNode* = object
    name*: string
    value*: Option[CExpr]

  IncludeNode* = object
    file*: string
    isSystem*: bool

  IfDefNode* = object
    name*: string
    body*: seq[CStmt]
    elseBody*: seq[CStmt]

  IfNotDefNode* = object
    name*: string
    body*: seq[CStmt]
    elseBody*: seq[CStmt]

  # Statements
  BlockStmtNode* = object
    statements*: seq[CStmt]

  ExprStmtNode* = object
    expression*: CExpr

  ReturnStmtNode* = object
    expression*: Option[CExpr]

  IfStmtNode* = object
    branches*: seq[IfBranchNode]
    elseBranch*: Option[CStmt]

  IfBranchNode* = object
    condition*: CExpr
    body*: CStmt

  WhileStmtNode* = object
    condition*: CExpr
    body*: CStmt

  # Expressions
  AssignmentNode* = object
    lhs*: CExpr
    rhs*: CExpr
    operator*: TokenKind

  BinaryExprNode* = object
    left*: CExpr
    operator*: TokenKind
    right*: CExpr

  UnaryExprNode* = object
    operator*: TokenKind
    operand*: CExpr

  FunctionCallNode* = object
    callee*: CExpr
    arguments*: seq[CExpr]

  IdentifierNode* = object
    name*: string

  ArrayAccessNode* = object
    array*: CExpr
    index*: CExpr

  GroupNode* = object
    expression*: CExpr

  AddressOfNode* = object
    operand*: CExpr

  DereferenceNode* = object
    operand*: CExpr

  # Struct literal
  StructLiteralMemberNode* = object
    name*: string
    value*: CExpr

  StructLiteralNode* = object
    typeName*: string
    members*: seq[StructLiteralMemberNode]

  # Array literal
  ArrayLiteralNode* = object
    elements*: seq[CExpr]

  # Member access
  MemberAccessNode* = object
    expr*: CExpr
    member*: string

  ArrowMemberAccessNode* = object
    expr*: CExpr
    member*: string

  # Literals
  IntLiteralNode* = object
    value*: int64

  UIntLiteralNode* = object
    value*: uint64

  FloatLiteralNode* = object
    value*: float

  StringLiteralNode* = object
    value*: string

  CharLiteralNode* = object
    value*: char

  BoolLiteralNode* = object
    value*: bool

  # Main CStmt type
  CStmt* = ref object
    pos*: Position
    comments*: seq[string]
    case kind*: CStmtKind
    of CskTranslationUnit: translationUnitNode*: TranslationUnitNode
    of CskFunctionDecl: functionDeclNode*: FunctionDeclNode
    of CskFunctionDef: functionDefNode*: FunctionDefNode
    of CskVarDecl: varDeclNode*: VarDeclNode
    of CskDefine: defineNode*: DefineNode
    of CskInclude: includeNode*: IncludeNode
    of CskIfDef: ifDefNode*: IfDefNode
    of CskIfNotDef: ifNotDefNode*: IfNotDefNode
    of CskBlockStmt: blockStmtNode*: BlockStmtNode
    of CskExprStmt: exprStmtNode*: ExprStmtNode
    of CskReturnStmt: returnStmtNode*: ReturnStmtNode
    of CskIfStmt: ifStmtNode*: IfStmtNode
    of CskWhileStmt: whileStmtNode*: WhileStmtNode
    of CskTypedef: typedefNode*: TypedefNode
    of CskStructDef: structDefNode*: StructDefNode
    of CskNop: discard

  # Main CExpr type
  CExpr* = ref object
    pos*: Position
    case kind*: CExprKind
    of CekAssignment: assignmentNode*: AssignmentNode
    of CekBinaryExpr: binaryExprNode*: BinaryExprNode
    of CekUnaryExpr: unaryExprNode*: UnaryExprNode
    of CekFunctionCall: functionCallNode*: FunctionCallNode
    of CekIdentifier: identifierNode*: IdentifierNode
    of CekArrayAccess: arrayAccessNode*: ArrayAccessNode
    of CekGroupExpr: groupNode*: GroupNode
    of CekAddressOf: addressOfNode*: AddressOfNode
    of CekDereference: dereferenceNode*: DereferenceNode
    of CekIntLiteral: intLiteralNode*: IntLiteralNode
    of CekUIntLiteral: uintLiteralNode*: UIntLiteralNode
    of CekFloatLiteral: floatLiteralNode*: FloatLiteralNode
    of CekStringLiteral: stringLiteralNode*: StringLiteralNode
    of CekCharLiteral: charLiteralNode*: CharLiteralNode
    of CekBoolLiteral: boolLiteralNode*: BoolLiteralNode
    of CekStructLiteral: structLiteralNode*: StructLiteralNode
    of CekMemberAccess: memberAccessNode*: MemberAccessNode
    of CekArrowMemberAccess: arrowMemberAccessNode*: ArrowMemberAccessNode
    of CekArrayLiteral: arrayLiteralNode*: ArrayLiteralNode
    of CekNullLiteral: discard
