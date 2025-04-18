import ../lexer/lexer
import ../types/[file_info, types, token, ast, annotation]
import ../reporter

import std/options
import std/random
import std/sequtils
import std/strutils
import std/tables

type
  PanicMode = ref object of ValueError ## Use error handling to recover from errors

  Parser* = object
    tokens*: seq[Token]
    current*: int
    file: FileInfo
    hasError*: bool

proc newParser*(file: FileInfo): Parser =
  ## Create a new parser for the given file
  var lexer = newLexer(file)
  let (hasError, tokens) = lexer.scan()
  result.tokens = tokens
  result.current = 0
  result.file = file
  result.hasError = hasError

proc parserError(parser: var Parser, error: string) =
  ## Log an error during parsing
  let current =
    if parser.current >= parser.tokens.len:
      parser.tokens.len - 1
    else:
      parser.current
  logError("Parser", error, parser.tokens[current].pos)
  parser.hasError = true
  raise PanicMode()

proc randomString(): string =
  ## Generate ids for code blocks
  ## This is a temporary solution, we should use a better way to generate unique ids
  const mySet = {'a' .. 'z', 'A' .. 'Z', '0' .. '9'}
  result = "b" & 12.newSeqWith(sample(mySet)).join("")

proc isAtEnd(parser: var Parser): bool {.inline.} =
  ## Check if the parser has reached the end of the tokens
  result = parser.current >= parser.tokens.len

proc peek(parser: var Parser, offset: int = 0): Option[Token] {.inline.} =
  ## Peek the current + offset token without advancing the parser
  ## Offset can be negative
  let index = parser.current + offset
  if index < 0 or index >= parser.tokens.len:
    return none(Token)
  return some(parser.tokens[index])

proc advance(parser: var Parser): Option[Token] {.inline.} =
  ## Advance the parser to the next token
  result = parser.peek()
  parser.current += 1

proc check(parser: var Parser, tokenKind: set[TokenKind]): bool {.inline.} =
  ## Check if the current token is of the given type
  if parser.isAtEnd():
    return false
  result = parser.peek().isSome and parser.peek().get().kind in tokenKind

proc match(parser: var Parser, tokenKind: set[TokenKind]): bool {.inline.} =
  ## Match the current token with the given type
  if parser.check(tokenKind):
    discard parser.advance()
    return true
  return false

proc consume(
    parser: var Parser, tokenKind: set[TokenKind], errorMessage: string
): Token =
  ## Consume the current token if it matches the given type
  if parser.check(tokenKind):
    return parser.advance().get()
  parser.parserError(errorMessage)

proc cleanUpNewLines(parser: var Parser) =
  ## Clean up new lines in the parser (there are times when we want to ignore new lines)
  while parser.check({TKNewline}):
    discard parser.advance()

proc synchronize(parser: var Parser) =
  ## Synchronize the parser to recover from errors
  discard parser.advance()
  while not parser.isAtEnd():
    # If we reach a semicolon/newline, we can stop synchronizing
    if parser.match({TKSemicolon, TKNewline}):
      return
    # If we reach a keyword that likely starts a new statement, we can stop synchronizing
    if parser.check({TKFun, TkVar, TkLet}):
      return
    # Otherwise, keep advancing until we find a statement boundary
    discard parser.advance()

# Forward declarations
proc parseExpression(parser: var Parser): Node
proc parseStatement(parser: var Parser, requireNewLine: bool = true): Node
proc parseBlockStmt(parser: var Parser): Node
proc parseType(parser: var Parser): Type

proc parsePrimary(parser: var Parser): Node =
  ## Parse a primary expression
  if parser.isAtEnd():
    parser.parserError("Unexpected end of input")
  if parser.match({TKIdent}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkIdentifier,
      pos: token.pos,
      identifierNode: IdentifierNode(name: token.lexeme),
    )
  elif parser.match({TKIntLit}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkIntLiteral,
      pos: token.pos,
      intLiteralNode: IntLiteralNode(value: token.intValue),
    )
  elif parser.match({TKUIntLit}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkUIntLiteral,
      pos: token.pos,
      uintLiteralNode: UIntLiteralNode(value: token.uintValue),
    )
  elif parser.match({TKFloatLit}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkFloatLiteral,
      pos: token.pos,
      floatLiteralNode: FloatLiteralNode(value: token.floatValue),
    )
  elif parser.match({TKStringLit}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkStringLiteral,
      pos: token.pos,
      stringLiteralNode: StringLiteralNode(value: token.lexeme),
    )
  elif parser.match({TKCStringLit}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkCStringLiteral,
      pos: token.pos,
      cStringLiteralNode: CStringLiteralNode(value: token.lexeme),
    )
  elif parser.match({TKCharLit}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkCharLiteral,
      pos: token.pos,
      charLiteralNode: CharLiteralNode(value: token.lexeme[0]),
    )
  elif parser.match({TkTrue, TkFalse}):
    let token = parser.peek(-1).get()
    return Node(
      kind: NkBoolLiteral,
      pos: token.pos,
      boolLiteralNode: BoolLiteralNode(value: token.lexeme == "true"),
    )
  elif parser.match({TKNil}):
    return Node(kind: NkNilLiteral, pos: parser.peek(-1).get().pos)
  elif parser.match({TKLParen}):
    let token = parser.peek(-1).get()
    let expr = parser.parseExpression()
    discard parser.consume({TKRParen}, "Expect ')' after expression.")
    return
      Node(kind: NkGroupExpr, pos: token.pos, groupNode: GroupNode(expression: expr))
  elif parser.match(Primitives):
    let token = parser.peek(-1).get()
    return Node(kind: NkType, pos: token.pos, typeNode: token.`type`)
  else:
    parser.parserError("Expect expression but got " & $parser.peek().get().kind)

proc parseFunctionArguments(parser: var Parser): seq[Node] =
  ## Parse function arguments (comma-separated expressions)
  result = @[]

  # Clean up new lines after open bracket
  parser.cleanUpNewLines()

  # Handle empty argument list
  if parser.check({TKRParen}):
    return result

  # Parse first argument
  result.add(parser.parseExpression())

  # Clean up new lines after first argument
  parser.cleanUpNewLines()

  # Parse any remaining arguments (comma-separated)
  while parser.match({TKComma}):
    result.add(parser.parseExpression())
    parser.cleanUpNewLines()

  return result

proc parseMemberAccess(parser: var Parser): Node =
  ## Parse member access (e.g., obj.member) and function calls (e.g., function())
  ## Handles Nim-style method call desugaring where a.fun(b) becomes fun(a, b)
  result = parser.parsePrimary()
  while parser.check({TKDot, TKLParen}):
    if parser.match({TKDot}):
      let token = parser.peek(-1).get()
      discard parser.consume(
        {TKIdent}, "Expect identifier after '.' but got " & $parser.peek().get().kind
      )
      let member = parser.peek(-1).get().lexeme

      # Check if this member access is immediately followed by a function call
      if parser.check({TKLParen}):
        # This is a method call like a.fun() that should become fun(a)
        discard parser.advance() # Consume the '('
        let args = parser.parseFunctionArguments()

        # Create a function call with the member as function name and object as first arg
        let methodName = Node(
          kind: NkIdentifier,
          pos: token.pos,
          identifierNode: IdentifierNode(name: member),
        )

        # Create a list with object as first argument followed by other args
        var fullArgs: seq[Node] = @[result]
        fullArgs.add(args)

        result = Node(
          kind: NkFunctionCall,
          pos: token.pos,
          functionCallNode: FunctionCallNode(callee: methodName, arguments: fullArgs),
        )

        discard parser.consume({TKRParen}, "Expect ')' after function arguments.")
      else:
        # Normal member access (not immediately followed by function call)
        result = Node(
          kind: NkMemberAccess,
          pos: token.pos,
          memberAccessNode: MemberAccessNode(obj: result, member: member),
        )
    elif parser.match({TKLParen}):
      let token = parser.peek(-1).get()
      let args = parser.parseFunctionArguments()
      result = Node(
        kind: NkFunctionCall,
        pos: token.pos,
        functionCallNode: FunctionCallNode(callee: result, arguments: args),
      )
      discard parser.consume({TKRParen}, "Expect ')' after function arguments.")

proc parseAddress(parser: var Parser): Node =
  ## Parse address-of operator (&)
  if parser.match({TKAmpersand}):
    let token = parser.peek(-1).get()
    let operand = parser.parseAddress()
    return Node(
      kind: NkAddressOfExpr,
      pos: token.pos,
      addressOfExprNode: AddressOfExprNode(operand: operand),
    )
  return parser.parseMemberAccess()

proc parseDereference(parser: var Parser): Node =
  ## Parse dereference operator (*)
  if parser.match({TKStar}):
    let token = parser.peek(-1).get()
    let operand = parser.parseDereference()
    return Node(
      kind: NkDerefExpr, pos: token.pos, derefExprNode: DerefExprNode(operand: operand)
    )
  return parser.parseAddress()

proc parseUnary(parser: var Parser): Node =
  ## Parse unary expressions (e.g., -x, !x)
  if parser.match({TKMinus, TKBang}):
    let token = parser.peek(-1).get()
    let operator = token
    let operand = parser.parseUnary()
    return Node(
      kind: NkUnaryExpr,
      pos: token.pos,
      unaryOpNode: UnaryOpNode(operator: operator.kind, operand: operand),
    )
  return parser.parseDereference()

proc parseMultiplicative(parser: var Parser): Node =
  ## Parse multiplicative expressions (e.g., x * y, x / y)
  var node = parser.parseUnary()
  while parser.match({TKStar, TKSlash, TKPercent}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseUnary()
    node = Node(
      kind: NkMultiplicativeExpr,
      pos: token.pos,
      binaryOpNode: BinaryOpNode(left: node, operator: operator.kind, right: right),
    )
  return node

proc parseAdditive(parser: var Parser): Node =
  ## Parse additive expressions (e.g., x + y, x - y)
  var node = parser.parseMultiplicative()
  while parser.match({TKPlus, TKMinus}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseMultiplicative()
    node = Node(
      kind: NkAdditiveExpr,
      pos: token.pos,
      binaryOpNode: BinaryOpNode(left: node, operator: operator.kind, right: right),
    )
  return node

proc parseComparison(parser: var Parser): Node =
  ## Parse comparison expressions (e.g., x < y, x > y)
  var node = parser.parseAdditive()
  while parser.match({TkRAngle, TKBiggerEqual, TkLAngle, TKSmallerEqual}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseAdditive()
    node = Node(
      kind: NkComparisonExpr,
      pos: token.pos,
      binaryOpNode: BinaryOpNode(left: node, operator: operator.kind, right: right),
    )
  return node

proc parseEquality(parser: var Parser): Node =
  ## Parse equality expressions (e.g., x == y, x != y)
  var node = parser.parseComparison()
  while parser.match({TKEqualEqual, TKBangEqual}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseComparison()
    node = Node(
      kind: NkEqualityExpr,
      pos: token.pos,
      binaryOpNode: BinaryOpNode(left: node, operator: operator.kind, right: right),
    )
  return node

proc parseLogical(parser: var Parser): Node =
  ## Parse logical expressions (e.g., x or y, x and y)
  var node = parser.parseEquality()
  while parser.match({TKOr, TKAnd}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseEquality()
    node = Node(
      kind: NkLogicalExpr,
      pos: token.pos,
      binaryOpNode: BinaryOpNode(left: node, operator: operator.kind, right: right),
    )
  return node

proc parseAssignment(parser: var Parser): Node =
  ## Parse assignment expressions (e.g., x.y = y)
  let left = parser.parseLogical()
  if parser.match({TKEqual}):
    let token = parser.peek(-1).get()
    let right = parser.parseAssignment()
    return Node(
      kind: NkAssignment,
      pos: token.pos,
      assignmentNode: AssignmentNode(identifier: left.identifierNode.name, value: right),
    )
  return left

proc parseExpression(parser: var Parser): Node =
  ## Parse an expression
  return parser.parseAssignment()

proc parseType(parser: var Parser): Type =
  ## Parse a type expression
  if parser.match({TKIdent}):
    let token = parser.peek(-1).get()
    return Type(kind: TkMeta, metaKind: MkUnresolved, name: token.lexeme)
  elif parser.match(Primitives + {TkCVarArgs}):
    let token = parser.peek(-1).get()
    return token.`type`
  elif parser.match({TkStar}):
    result = newPointerType(parser.parseType(), false)
  elif parser.match({TokenKind.TkROPointer}):
    result = newPointerType(parser.parseType(), true)
  else:
    echo parser.peek().get().kind
    parser.parserError("Expect type.")

proc parseExpressionStmt(parser: var Parser): Node =
  ## Parse an expression statement
  let expr = parser.parseExpression()
  return
    Node(kind: NkExprStmt, pos: expr.pos, exprStmtNode: ExprStmtNode(expression: expr))

proc parseReturnStmt(parser: var Parser): Node =
  ## Parse a return statement
  let token = parser.peek(-1).get()
  let expr =
    if parser.check({TKSemicolon, TKNewline}):
      none(Node)
    else:
      some(parser.parseExpression())
  return Node(
    kind: NkReturnStmt, pos: token.pos, returnStmtNode: ReturnStmtNode(expression: expr)
  )

proc parseBlockStmt(parser: var Parser): Node =
  ## Parse a block statement
  let token = parser.peek(-1).get()
  var statements: seq[Node]
  while not parser.check({TKRBrace}):
    statements.add(parser.parseStatement())
  discard parser.consume({TKRBrace}, "Expect '}' after block.")
  return Node(
    kind: NkBlockStmt,
    pos: token.pos,
    blockStmtNode: BlockStmtNode(statements: statements, blockId: randomString()),
  )

proc parseVarDecl(parser: var Parser, isPublic: bool, isReadOnly: bool = false): Node =
  ## Parse a variable declaration
  let token = parser.peek(-1).get()
  discard parser.consume(
    {TKIdent}, "Expect identifier after '" & (if isReadOnly: "let" else: "var") & "'."
  )
  let identifier = parser.peek(-1).get().lexeme
  let typeAnnotation =
    if parser.match({TKColon}):
      parser.parseType()
    else:
      Type(kind: TkMeta, metaKind: MkToInfer)
  let initializer =
    if parser.match({TKEqual}):
      some(parser.parseExpression())
    else:
      none(Node)
  return Node(
    kind: NkVarDecl,
    pos: token.pos,
    varDeclNode: VarDeclNode(
      identifier: identifier,
      typeAnnotation: typeAnnotation,
      initializer: initializer,
      isPublic: isPublic,
      isReadOnly: isReadOnly,
    ),
  )

proc parseFunDecl(parser: var Parser, isPublic: bool): Node =
  ## Parse a function declaration
  let token = parser.peek(-1).get()
  discard parser.consume({TKIdent}, "Expect identifier after 'fun'.")
  let identifier = parser.peek(-1).get().lexeme
  discard parser.consume({TKLParen}, "Expect '(' after function name.")

  # Clean up new lines after opening parenthesis
  parser.cleanUpNewLines()

  var parameters: seq[ParameterNode]
  while not parser.check({TKRParen}):
    if parameters.len > 0:
      discard parser.consume({TKComma}, "Expect ',' between parameters.")
      # Clean up new lines after comma
      parser.cleanUpNewLines()

    discard parser.consume({TKIdent}, "Expect parameter name.")
    let paramName = parser.peek(-1).get().lexeme
    discard parser.consume({TKColon}, "Expect ':' after parameter name.")
    let paramType = parser.parseType()
    parameters.add(ParameterNode(identifier: paramName, paramType: paramType))

    # Clean up new lines after parameter
    parser.cleanUpNewLines()

  discard parser.consume(
    {TKRParen},
    "Expect ')' after function parameters but got " & $parser.peek().get().kind,
  )
  let returnType =
    if parser.match({TKColon}):
      parser.parseType()
    else:
      Type(kind: TkPrimitive, primitive: Void)
  let body =
    if parser.check({TKLBrace}):
      some(parser.parseStatement())
    else:
      none(Node)
  return Node(
    kind: NkFunDecl,
    pos: token.pos,
    funDeclNode: FunDeclNode(
      identifier: identifier,
      parameters: parameters,
      returnType: returnType,
      body: body,
      isPublic: isPublic,
    ),
  )

proc parseAnnotationProperties(
    parser: var Parser
): tuple[name: string, value: Option[string]] =
  ## Parse an annotation argument (name=value or just an identifier as flag)
  var name: string
  var value: Option[string]

  # Case 1: identifier=value format
  if parser.check({TKIdent}) and parser.peek(1).isSome and
      parser.peek(1).get().kind == TKEqual:
    name = parser.consume({TKIdent}, "Expect argument name").lexeme
    discard parser.consume({TKEqual}, "Expect '=' after argument name")

    # Parse the argument value (must be a literal)
    if parser.match({TKIntLit, TKFloatLit, TKStringLit, TKNil}):
      let token = parser.peek(-1).get()
      case token.kind
      of TKIntLit:
        value = some($token.intValue)
      of TKFloatLit:
        value = some($token.floatValue)
      of TKStringLit:
        value = some($token.stringValue)
      of TKNil:
        value = some("nil")
      else:
        discard # Can't happen due to the match pattern
    else:
      parser.parserError("Expect literal value for annotation argument")

  # Case 2: just an identifier (flag argument)
  elif parser.match({TKIdent}):
    name = parser.peek(-1).get().lexeme
    # Flag argument has no value
    value = none(string)
  else:
    parser.parserError("Expect identifier or name=value pair in annotation argument")

  result.name = name
  result.value = value

proc parseAnnotation(parser: var Parser): Annotation =
  ## Parse an annotation like #[name] or #[name(arg1, arg2=value)]
  let startToken = parser.peek(-1).get() # The #[ token

  # Parse the annotation name
  discard parser.consume({TKIdent}, "Expect annotation name")
  let name = parser.peek(-1).get().lexeme

  # Parse optional arguments within parentheses
  var properties: Table[string, Option[string]] = initTable[string, Option[string]]()
  if parser.match({TKLParen}):
    # If there are arguments
    if not parser.check({TKRParen}):
      # Parse first argument
      let (argName, argValue) = parser.parseAnnotationProperties()
      properties[argName] = argValue

      # Parse remaining arguments (comma-separated)
      while parser.match({TKComma}):
        let (argName, argValue) = parser.parseAnnotationProperties()
        properties[argName] = argValue

    discard parser.consume({TKRParen}, "Expect ')' after annotation arguments")

  discard parser.consume({TKRBracket}, "Expect ']' after annotation")

  return Annotation(name: name, properties: properties)

proc parseAnnotations(parser: var Parser): Table[string, Annotation] =
  ## Parse multiple annotations

  while true: #
    # check for the start of an annotation TkHash + TkLBracket
    let first = parser.peek()
    if first.isNone or first.get().kind != TKHash:
      return
    # Consume the TkHash token
    discard parser.advance()
    # Check for the opening bracket
    discard parser.consume({TKLBracket}, "Expect '[' after '#'")
    # Parse the annotation
    let annotation = parser.parseAnnotation()
    result[annotation.name] = annotation
    # Expect newLine or semicolon after annotation
    discard parser.consume(
      {TKNewline, TKSemicolon}, "Expect new line or semicolon after annotation"
    )
    # Clean up new lines
    parser.cleanUpNewLines()

proc parseIfStmt(parser: var Parser): Node =
  ## Parse an if statement with optional elif and else branches
  let ifToken = parser.peek(-1).get() # 'if' token already matched
  discard parser.consume({TKLParen}, "Expect '(' after 'if'.")
  let ifCond = parser.parseExpression()
  discard parser.consume({TKRParen}, "Expect ')' after if condition.")
  let ifBody = parser.parseStatement(false)
  var branches: seq[IfBranchNode] = @[IfBranchNode(scopeId: randomString(), condition: ifCond, body: ifBody)]

  # Parse zero or more elif branches
  while parser.match({TKElif}):
    let elifToken = parser.peek(-1).get()
    discard parser.consume({TKLParen}, "Expect '(' after 'elif'.")
    let elifCond = parser.parseExpression()
    discard parser.consume({TKRParen}, "Expect ')' after elif condition.")
    let elifBody = parser.parseStatement(false)
    branches.add(IfBranchNode(scopeId: randomString(), condition: elifCond, body: elifBody))

  # Optionally parse else branch
  var elseBranch: Option[ElseBranchNode] = none(ElseBranchNode)
  if parser.match({TKElse}):
    let elseBody = parser.parseStatement(false)
    elseBranch = some(ElseBranchNode(scopeId: randomString(), body: elseBody))

  return Node(
    kind: NkIfStmt,
    pos: ifToken.pos,
    ifStmtNode: IfStmtNode(branches: branches, elseBranch: elseBranch)
  )

proc parseStatement(parser: var Parser, requireNewLine: bool = true): Node =
  ## Parse a statement (flexible preamble: comments, annotations, newlines in any order)
  try:
    var comments: seq[string]
    var annotationsTable: Table[string, Annotation] = initTable[string, Annotation]()

    # Flexible preamble: loop until a statement token is found
    while true:
      if parser.check({TKComment}):
        let token = parser.peek().get()
        comments.add(token.lexeme)
        discard parser.advance()
      elif parser.check({TKNewline, TKSemicolon}):
        discard parser.advance()
      elif parser.check({TKHash}):
        # Parse all consecutive annotations
        let anns = parser.parseAnnotations()
        for k, v in anns:
          annotationsTable[k] = v
      else:
        break

    var pub = false
    if parser.match({TKPub}):
      pub = true
    if parser.match({TKVar}):
      result = parser.parseVarDecl(pub, false)
    elif parser.match({TKLet}):
      result = parser.parseVarDecl(pub, true)
    elif parser.match({TKFun}):
      result = parser.parseFunDecl(pub)
    elif parser.match({TKReturn}):
      result = parser.parseReturnStmt()
    elif parser.match({TKLBrace}):
      result = parser.parseBlockStmt()
    elif parser.match({TKIf}):
      result = parser.parseIfStmt()
    else:
      result = parser.parseExpressionStmt()

    result.annotations = Annotations(annotationsTable)
    result.comments = comments

    if not parser.isAtEnd() and requireNewLine:
      discard
        parser.consume({TKSemicolon, TKNewline}, "Expect ';' or '\\n' after statement.")
  except PanicMode:
    # Synchronize to recover from errors
    parser.synchronize()
    return Node(kind: NkNop)

proc parseModule*(file: FileInfo): tuple[hasError: bool, module: Node] =
  ## Parse a module
  var parser = newParser(file)
  var statements: seq[Node]
  while not parser.isAtEnd():
    statements.add(parser.parseStatement())
  result.hasError = parser.hasError
  result.module = Node(
    kind: NkModule, moduleNode: ModuleNode(name: file.name, statements: statements)
  )
