import position
import types
import token
import annotation
import std/options

type
  NodeKind* = enum
    # Program structure
    NkModule
    NkNop

    # Statements
    NkVarDecl # Now represents both var and let declarations
    NkFunDecl
    NkBlockStmt
    NkExprStmt
    NkReturnStmt
    NkIfStmt

    # Expressions
    NkAssignment
    NkLogicalExpr
    NkEqualityExpr
    NkComparisonExpr
    NkAdditiveExpr
    NkMultiplicativeExpr
    NkUnaryExpr
    NkMemberAccess
    NkFunctionCall
    NkIdentifier
    NkGroupExpr
    NkAddressOfExpr
    NkDerefExpr

    # Literals
    NkIntLiteral
    NkUIntLiteral
    NkFloatLiteral
    NkStringLiteral
    NkCStringLiteral
    NkCharLiteral
    NkBoolLiteral
    NkNilLiteral

    # Types
    NkType

  # Program
  ModuleNode* = object
    name*: string
    statements*: seq[Node]

  # Statements
  VarDeclNode* = object
    isPublic*: bool
    isReadOnly*: bool # Indicates if this is a 'let' declaration
    identifier*: string
    typeAnnotation*: Type
    initializer*: Option[Node]

  FunDeclNode* = object
    identifier*: string
    parameters*: seq[ParameterNode]
    returnType*: Type
    body*: Option[Node]
    isPublic*: bool

  ParameterNode* = object
    identifier*: string
    paramType*: Type

  BlockStmtNode* = object
    blockId*: string
    statements*: seq[Node]

  ExprStmtNode* = object
    expression*: Node

  ReturnStmtNode* = object
    expression*: Option[Node]

  IfStmtNode* = object
    branches*: seq[IfBranchNode]
    elseBranch*: Option[ElseBranchNode]

  IfBranchNode* = object
    scopeId*: string
    condition*: Node
    body*: Node

  ElseBranchNode* = object
    scopeId*: string
    body*: Node

  # Expressions
  AssignmentNode* = object
    identifier*: string
    value*: Node

  BinaryOpNode* = object
    left*: Node
    operator*: TokenKind
    right*: Node

  UnaryOpNode* = object
    operator*: TokenKind
    operand*: Node

  MemberAccessNode* = object
    obj*: Node
    member*: string

  FunctionCallNode* = object
    callee*: Node
    arguments*: seq[Node]

  IdentifierNode* = object
    name*: string

  GroupNode* = object
    expression*: Node

  AddressOfExprNode* = object
    operand*: Node

  DerefExprNode* = object
    operand*: Node

  # Literals
  IntLiteralNode* = object
    value*: int

  UIntLiteralNode* = object
    value*: uint

  FloatLiteralNode* = object
    value*: float

  StringLiteralNode* = object
    value*: string

  CStringLiteralNode* = object
    value*: string

  CharLiteralNode* = object
    value*: char

  BoolLiteralNode* = object
    value*: bool

  # Types
  PrimitiveTypeNode* = object
    kind*: TokenKind

  # Main Node type
  Node* = ref object
    pos*: Position # Source position for error reporting
    annotations*: Annotations
    comments*: seq[string]
    case kind*: NodeKind
    of NkModule: moduleNode*: ModuleNode
    of NkVarDecl: varDeclNode*: VarDeclNode
    of NkFunDecl: funDeclNode*: FunDeclNode
    of NkBlockStmt: blockStmtNode*: BlockStmtNode
    of NkExprStmt: exprStmtNode*: ExprStmtNode
    of NkReturnStmt: returnStmtNode*: ReturnStmtNode
    of NkIfStmt: ifStmtNode*: IfStmtNode
    of NkAssignment: assignmentNode*: AssignmentNode
    of NkLogicalExpr, NkEqualityExpr, NkComparisonExpr, NkAdditiveExpr,
        NkMultiplicativeExpr: binaryOpNode*: BinaryOpNode
    of NkUnaryExpr: unaryOpNode*: UnaryOpNode
    of NkMemberAccess: memberAccessNode*: MemberAccessNode
    of NkFunctionCall: functionCallNode*: FunctionCallNode
    of NkIdentifier: identifierNode*: IdentifierNode
    of NkGroupExpr: groupNode*: GroupNode
    of NkAddressOfExpr: addressOfExprNode*: AddressOfExprNode
    of NkDerefExpr: derefExprNode*: DerefExprNode
    of NkIntLiteral: intLiteralNode*: IntLiteralNode
    of NkUIntLiteral: uintLiteralNode*: UIntLiteralNode
    of NkFloatLiteral: floatLiteralNode*: FloatLiteralNode
    of NkStringLiteral: stringLiteralNode*: StringLiteralNode
    of NkCStringLiteral: cStringLiteralNode*: CStringLiteralNode
    of NkCharLiteral: charLiteralNode*: CharLiteralNode
    of NkBoolLiteral: boolLiteralNode*: BoolLiteralNode
    of NkNilLiteral, NkNop: discard
    of NkType: typeNode*: Type

# Node constructors
proc newModule*(pos: Position, statements: seq[Node]): Node =
  Node(kind: NkModule, pos: pos, moduleNode: ModuleNode(statements: statements))
