import position
import types
import token
import std/options

type
  NodeKind* = enum
    # Program structure
    NkModule
    NkNop

    # Statements
    NkVarDecl
    NkLetDecl
    NkFunDecl
    NkBlockStmt
    NkExprStmt
    NkReturnStmt

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

    # Literals
    NkIntLiteral
    NkUIntLiteral
    NkFloatLiteral
    NkStringLiteral
    NkCStringLiteral
    NkCharLiteral
    NkBoolLiteral
    NkNilLiteral
    NkCommentLiteral # Useful for generated code, maybe

    # Types
    NkType

  # Program
  ModuleNode* = object
    name*: string
    statements*: seq[Node]

  # Statements
  VarDeclNode* = object
    isPublic*: bool
    identifier*: string
    typeAnnotation*: Type
    initializer*: Option[Node]
    annotations*: seq[Annotation]

  LetDeclNode* = object
    isPublic*: bool
    identifier*: string
    typeAnnotation*: Type
    initializer*: Option[Node]
    annotations*: seq[Annotation]

  FunDeclNode* = object
    identifier*: string
    parameters*: seq[ParameterNode]
    returnType*: Type
    body*: Option[Node]
    isPublic*: bool
    annotations*: seq[Annotation]

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

  CommentLiteralNode* = object
    value*: string

  # Types
  PrimitiveTypeNode* = object
    kind*: TokenKind

  # Annotations
  AnnotationArgNode* = object
    name*: string
    value*: Option[Node]

  Annotation* = object
    name*: string
    args*: seq[AnnotationArgNode]

  # Main Node type
  Node* = ref object
    pos*: Position # Source position for error reporting
    `type`*: Type # Type of the node, void for statements
    case kind*: NodeKind
    of NkModule: moduleNode*: ModuleNode
    of NkVarDecl: varDeclNode*: VarDeclNode
    of NkLetDecl: letDeclNode*: LetDeclNode
    of NkFunDecl: funDeclNode*: FunDeclNode
    of NkBlockStmt: blockStmtNode*: BlockStmtNode
    of NkExprStmt: exprStmtNode*: ExprStmtNode
    of NkReturnStmt: returnStmtNode*: ReturnStmtNode
    of NkAssignment: assignmentNode*: AssignmentNode
    of NkLogicalExpr, NkEqualityExpr, NkComparisonExpr, NkAdditiveExpr,
        NkMultiplicativeExpr: binaryOpNode*: BinaryOpNode
    of NkUnaryExpr: unaryOpNode*: UnaryOpNode
    of NkMemberAccess: memberAccessNode*: MemberAccessNode
    of NkFunctionCall: functionCallNode*: FunctionCallNode
    of NkIdentifier: identifierNode*: IdentifierNode
    of NkGroupExpr: groupNode*: GroupNode
    of NkIntLiteral: intLiteralNode*: IntLiteralNode
    of NkUIntLiteral: uintLiteralNode*: UIntLiteralNode
    of NkFloatLiteral: floatLiteralNode*: FloatLiteralNode
    of NkStringLiteral: stringLiteralNode*: StringLiteralNode
    of NkCStringLiteral: cStringLiteralNode*: CStringLiteralNode
    of NkCharLiteral: charLiteralNode*: CharLiteralNode
    of NkBoolLiteral: boolLiteralNode*: BoolLiteralNode
    of NkNilLiteral, NkNop: discard
    of NkType: typeNode*: Type
    of NkCommentLiteral: commentLiteralNode*: CommentLiteralNode

# Node constructors
proc newModule*(pos: Position, statements: seq[Node]): Node =
  Node(kind: NkModule, pos: pos, moduleNode: ModuleNode(statements: statements))

# Annotation helper functions
proc hasAnnotation*(annotations: seq[Annotation], name: string): bool =
  ## Checks if a specific annotation exists in the given annotations list
  for ann in annotations:
    if ann.name == name:
      return true
  return false

proc getAnnotation*(annotations: seq[Annotation], name: string): Option[Annotation] =
  ## Gets a specific annotation from the annotations list if it exists
  for ann in annotations:
    if ann.name == name:
      return some(ann)
  return none(Annotation)

proc getAnnotationArg*(annotation: Annotation, name: string): Option[AnnotationArgNode] =
  ## Gets a specific argument from an annotation if it exists
  for arg in annotation.args:
    if arg.name == name:
      return some(arg)
  return none(AnnotationArgNode)

proc getAnnotationArgValue*(annotations: seq[Annotation], annotationName, argName: string): Option[Node] =
  ## Gets a specific annotation argument's value if both the annotation and argument exist
  let annOpt = getAnnotation(annotations, annotationName)
  if annOpt.isNone:
    return none(Node)
    
  let argOpt = getAnnotationArg(annOpt.get, argName)
  if argOpt.isNone or argOpt.get.value.isNone:
    return none(Node)
    
  return argOpt.get.value

