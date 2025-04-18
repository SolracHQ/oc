import std/options
import std/sets
import position
import types_c
import token

type
  CNodeKind* = enum
    # Program structure
    CnkTranslationUnit # Top-level C/H file
    CnkNop # No operation

    # Declarations
    CnkFunctionDecl # Function declaration
    CnkFunctionDef # Function definition
    CnkVarDecl # Variable declaration (includes const, static, etc.)

    # Preprocessor
    CnkDefine # #define directive
    CnkInclude # #include directive
    CnkIfDef # #ifdef directive
    CnkIfNotDef # #ifndef directive

    # Statements
    CnkBlockStmt # Block of statements (compound statement)
    CnkExprStmt # Expression statement
    CnkReturnStmt # Return statement
    CnkIfStmt # If statement

    # Expressions
    CnkAssignment # Assignment expression
    CnkBinaryExpr # Binary expression (+, -, *, /, etc.)
    CnkUnaryExpr # Unary expression (!, -, etc.)
    CnkFunctionCall # Function call
    CnkIdentifier # Identifier reference
    CnkArrayAccess # Array access
    CnkGroupExpr # Parenthesized expression
    CnkAddressOf # Address-of operator (&)
    CnkDereference # Dereference operator (*)

    # Literals
    CnkIntLiteral # Integer literal
    CnkUIntLiteral # Unsigned integer literal
    CnkFloatLiteral # Float literal
    CnkStringLiteral # String literal
    CnkCharLiteral # Character literal
    CnkBoolLiteral # Boolean literal (true/false)
    CnkNullLiteral # NULL literal

  # Program structure
  TranslationUnitNode* = object
    includes*: HashSet[IncludeNode] #include directives
    declarations*: seq[CNode] # Function declarations and definitions

  # Declarations
  FunctionDeclNode* = object
    name*: string
    returnType*: CType
    parameters*: seq[ParameterNode]
    isStatic*: bool
    isExtern*: bool
    isInline*: bool

  FunctionDefNode* = object
    declaration*: CNode # Must be CnkFunctionDecl
    body*: CNode # Must be CnkBlockStmt

  ParameterNode* = object
    name*: string
    paramType*: CType

  VarDeclNode* = object
    name*: string
    varType*: CType
    initializer*: Option[CNode]
    isConst*: bool # const modifier
    isStatic*: bool # static modifier
    isExtern*: bool # extern modifier
    isVolatile*: bool # volatile modifier

  # Preprocessor
  DefineNode* = object
    name*: string
    value*: Option[CNode] # For object-like macros, None for macros without a value

  IncludeNode* = object
    file*: string
    isSystem*: bool # System include (angle brackets) or user include (quotes)

  IfDefNode* = object
    name*: string
    body*: seq[CNode] # Body of the #ifdef directive
    elseBody*: seq[CNode] # Body of the #else directive

  IfNotDefNode* = object
    name*: string
    body*: seq[CNode] # Body of the #ifndef directive
    elseBody*: seq[CNode] # Body of the #else directive

  # Statements
  BlockStmtNode* = object
    statements*: seq[CNode]

  ExprStmtNode* = object
    expression*: CNode

  ReturnStmtNode* = object
    expression*: Option[CNode] # None for "return;"

  IfStmtNode* = object
    branches*: seq[IfBranchNode] # List of if branches
    elseBranch*: Option[CNode] # Optional else branch

  IfBranchNode* = object
    condition*: CNode # Condition for the if branch
    body*: CNode # Body of the if branch

  # Expressions
  AssignmentNode* = object
    lhs*: CNode # Left-hand side (target)
    rhs*: CNode # Right-hand side (value)
    operator*: TokenKind # =, +=, -=, etc.

  BinaryExprNode* = object
    left*: CNode
    operator*: TokenKind
    right*: CNode

  UnaryExprNode* = object
    operator*: TokenKind
    operand*: CNode

  FunctionCallNode* = object
    callee*: CNode
    arguments*: seq[CNode]

  IdentifierNode* = object
    name*: string

  ArrayAccessNode* = object
    array*: CNode
    index*: CNode

  GroupNode* = object
    expression*: CNode

  AddressOfNode* = object
    operand*: CNode

  DereferenceNode* = object
    operand*: CNode

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

  # Main CNode type
  CNode* = ref object
    pos*: Position # Source position for error reporting
    comments*: seq[string] # Comments associated with this node
    case kind*: CNodeKind
    of CnkTranslationUnit: translationUnitNode*: TranslationUnitNode
    of CnkFunctionDecl: functionDeclNode*: FunctionDeclNode
    of CnkFunctionDef: functionDefNode*: FunctionDefNode
    of CnkVarDecl: varDeclNode*: VarDeclNode
    of CnkDefine: defineNode*: DefineNode
    of CnkInclude: includeNode*: IncludeNode
    of CnkIfDef: ifDefNode*: IfDefNode
    of CnkIfNotDef: ifNotDefNode*: IfNotDefNode
    of CnkBlockStmt: blockStmtNode*: BlockStmtNode
    of CnkExprStmt: exprStmtNode*: ExprStmtNode
    of CnkReturnStmt: returnStmtNode*: ReturnStmtNode
    of CnkIfStmt: ifStmtNode*: IfStmtNode
    of CnkAssignment: assignmentNode*: AssignmentNode
    of CnkBinaryExpr: binaryExprNode*: BinaryExprNode
    of CnkUnaryExpr: unaryExprNode*: UnaryExprNode
    of CnkFunctionCall: functionCallNode*: FunctionCallNode
    of CnkIdentifier: identifierNode*: IdentifierNode
    of CnkArrayAccess: arrayAccessNode*: ArrayAccessNode
    of CnkGroupExpr: groupNode*: GroupNode
    of CnkAddressOf: addressOfNode*: AddressOfNode
    of CnkDereference: dereferenceNode*: DereferenceNode
    of CnkIntLiteral: intLiteralNode*: IntLiteralNode
    of CnkUIntLiteral: uintLiteralNode*: UIntLiteralNode
    of CnkFloatLiteral: floatLiteralNode*: FloatLiteralNode
    of CnkStringLiteral: stringLiteralNode*: StringLiteralNode
    of CnkCharLiteral: charLiteralNode*: CharLiteralNode
    of CnkBoolLiteral: boolLiteralNode*: BoolLiteralNode
    of CnkNullLiteral, CnkNop: discard
