import ../lexer/lexer
import ../types/[file_info, types, token, ast, annotation]
import ../reporter
import ../builders/ast_builder

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

proc parserError(parser: var Parser, error: string, hint: string = "") =
  ## Log an error during parsing
  let current =
    if parser.current >= parser.tokens.len:
      parser.tokens.len - 1
    else:
      parser.current
  logError("Parser", parser.tokens[current].pos, error, hint)
  parser.hasError = true
  raise PanicMode()

proc randomID(): string =
  ## Generate ids for code blocks
  ## This is a temporary solution, we should use a better way to generate unique ids
  const mySet = {'a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'}
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
    if parser.check({TKFun, TkVar, TkLet, TkIf, TKWhile, TKReturn}):
      return
    # Otherwise, keep advancing until we find a statement boundary
    discard parser.advance()

# Forward declarations
proc parseExpression(parser: var Parser): Expr
proc parseStatement(parser: var Parser, requireNewLine: bool = true): Stmt
proc parseBlockStmt(parser: var Parser): Stmt
proc parseType(parser: var Parser): Type

proc parseStructLiteral(parser: var Parser): Expr =
  ## Parse a struct literal: TypeName { member1: expr1, member2: expr2, ... }
  let typeToken = parser.peek(-1).get() # Already matched identifier
  let typeName = typeToken.lexeme
  discard
    parser.consume({TKLBrace}, "Expect '{' after struct type name for struct literal")
  var members: seq[StructLiteralMember] = @[]
  parser.cleanUpNewLines()
  if not parser.check({TKRBrace}):
    while true:
      let nameTok = parser.consume({TKIdent}, "Expect member name in struct literal")
      let memberName = nameTok.lexeme
      let memberNamePos = nameTok.pos
      discard
        parser.consume({TKColon}, "Expect ':' after member name in struct literal")
      let valueExpr = parser.parseExpression()
      members.add(newStructLiteralMember(memberName, memberNamePos, valueExpr))
      parser.cleanUpNewLines()
      if not parser.match({TKComma}):
        break
      parser.cleanUpNewLines()
  discard parser.consume({TKRBrace}, "Expect '}' after struct literal")
  return newStructLiteralExpr(typeName, members, typeToken.pos)

proc parsePrimary(parser: var Parser): Expr =
  ## Parse a primary expression
  if parser.isAtEnd():
    parser.parserError("Unexpected end of input")
  if parser.match({TKIdent}):
    let token = parser.peek(-1).get()
    # Check for struct literal: Identifier '{'
    if parser.check({TKLBrace}):
      return parser.parseStructLiteral()
    return newIdentifierExpr(token.lexeme, token.pos)
  elif parser.match({TKIntLit}):
    let token = parser.peek(-1).get()
    return newLiteralExpr(token.intValue, token.pos)
  elif parser.match({TKUIntLit}):
    let token = parser.peek(-1).get()
    return newLiteralExpr(token.uintValue, token.pos)
  elif parser.match({TKFloatLit}):
    let token = parser.peek(-1).get()
    return newLiteralExpr(token.floatValue, token.pos)
  elif parser.match({TKStringLit}):
    let token = parser.peek(-1).get()
    return newLiteralExpr(token.stringValue, token.pos)
  elif parser.match({TKCStringLit}):
    let token = parser.peek(-1).get()
    return newCStringLiteralExpr(token.lexeme, token.pos)
  elif parser.match({TKCharLit}):
    let token = parser.peek(-1).get()
    return newLiteralExpr(token.charValue, token.pos)
  elif parser.match({TkTrue, TkFalse}):
    let token = parser.peek(-1).get()
    return newLiteralExpr(token.lexeme == "true", token.pos)
  elif parser.match({TKNil}):
    let token = parser.peek(-1).get()
    return newNilLiteralExpr(token.pos)
  elif parser.match({TKLParen}):
    let token = parser.peek(-1).get()
    let expr = parser.parseExpression()
    discard parser.consume({TKRParen}, "Expect ')' after expression.")
    return newGroupExpr(expr, token.pos)
  else:
    parser.parserError("Expect expression but got " & $parser.peek().get().kind)

proc parseFunctionArguments(parser: var Parser): seq[Expr] =
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

proc parseMemberAccess(parser: var Parser): Expr =
  ## Parse member access (e.g., obj.member) and function calls (e.g., function()) and function pointer calls (e.g., obj.fun())
  var expr = parser.parsePrimary()
  while true:
    if parser.match({TKDot}):
      let dotToken = parser.peek(-1).get()
      let memberToken =
        parser.consume({TKIdent}, "Expect identifier after '.' for member access")
      expr = newMemberAccessExpr(expr, memberToken.lexeme, dotToken.pos)
    elif parser.match({TKLParen}):
      let lparenToken = parser.peek(-1).get()
      let args = parser.parseFunctionArguments()
      discard parser.consume({TKRParen}, "Expect ')' after function arguments")
      expr = newFunctionCallExpr(expr, args, lparenToken.pos)
    else:
      break
  return expr

proc parseAddress(parser: var Parser): Expr =
  ## Parse address-of operator (&)
  if parser.match({TKAmpersand}):
    let token = parser.peek(-1).get()
    let operand = parser.parseAddress()
    return newAddressOfExpr(operand, token.pos)
  return parser.parseMemberAccess()

proc parseDereference(parser: var Parser): Expr =
  ## Parse dereference operator (*)
  if parser.match({TKStar}):
    let token = parser.peek(-1).get()
    let operand = parser.parseDereference()
    return newDerefExpr(operand, token.pos)
  return parser.parseAddress()

proc parseUnary(parser: var Parser): Expr =
  ## Parse unary expressions (e.g., -x, !x)
  if parser.match({TKMinus, TKBang}):
    let token = parser.peek(-1).get()
    let operator = token
    let operand = parser.parseUnary()
    return newUnaryOpExpr(operator.kind, operand, token.pos)
  return parser.parseDereference()

proc parseMultiplicative(parser: var Parser): Expr =
  ## Parse multiplicative expressions (e.g., x * y, x / y)
  var node = parser.parseUnary()
  while parser.match({TKStar, TKSlash, TKPercent}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseUnary()
    node = newBinaryOpExpr(EkMultiplicativeExpr, node, operator.kind, right, token.pos)
  return node

proc parseAdditive(parser: var Parser): Expr =
  ## Parse additive expressions (e.g., x + y, x - y)
  var node = parser.parseMultiplicative()
  while parser.match({TKPlus, TKMinus}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseMultiplicative()
    node = newBinaryOpExpr(EkAdditiveExpr, node, operator.kind, right, token.pos)
  return node

proc parseComparison(parser: var Parser): Expr =
  ## Parse comparison expressions (e.g., x < y, x > y)
  var node = parser.parseAdditive()
  while parser.match({TkRAngle, TKBiggerEqual, TkLAngle, TKSmallerEqual}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseAdditive()
    node = newBinaryOpExpr(EkComparisonExpr, node, operator.kind, right, token.pos)
  return node

proc parseEquality(parser: var Parser): Expr =
  ## Parse equality expressions (e.g., x == y, x != y)
  var node = parser.parseComparison()
  while parser.match({TKEqualEqual, TKBangEqual}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseComparison()
    node = newBinaryOpExpr(EkEqualityExpr, node, operator.kind, right, token.pos)
  return node

proc parseLogical(parser: var Parser): Expr =
  ## Parse logical expressions (e.g., x or y, x and y)
  var node = parser.parseEquality()
  while parser.match({TKOr, TKAnd}):
    let token = parser.peek(-1).get()
    let operator = token
    let right = parser.parseEquality()
    node = newBinaryOpExpr(EkLogicalExpr, node, operator.kind, right, token.pos)
  return node

proc parseAssignment(parser: var Parser): Expr =
  ## Parse assignment expressions (e.g., x.y = y)
  let left = parser.parseLogical()
  if parser.match({TKEqual}):
    let token = parser.peek(-1).get()
    let right = parser.parseAssignment()
    return newAssignmentExpr(left, right, token.pos)
  return left

proc parseExpression(parser: var Parser): Expr =
  ## Parse an expression
  return parser.parseAssignment()

proc parseType(parser: var Parser): Type =
  ## Parse a type expression
  if parser.match({TKIdent}):
    let token = parser.peek(-1).get()
    return Type(kind: TkMeta, metaKind: MkNamedType, name: token.lexeme)
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

proc parseExpressionStmt(parser: var Parser): Stmt =
  ## Parse an expression statement
  let expr = parser.parseExpression()
  return newExprStmt(expr, expr.pos)

proc parseReturnStmt(parser: var Parser): Stmt =
  ## Parse a return statement
  let token = parser.peek(-1).get()
  let expr =
    if parser.check({TKSemicolon, TKNewline}):
      none(Expr)
    else:
      some(parser.parseExpression())
  return newReturnStmt(expr, token.pos)

proc parseBlockStmt(parser: var Parser): Stmt =
  ## Parse a block statement
  let token = parser.peek(-1).get()
  var statements: seq[Stmt]
  while not parser.check({TKRBrace}):
    statements.add(parser.parseStatement())
  discard parser.consume({TKRBrace}, "Expect '}' after block.")
  return newBlockStmt(randomID(), statements, token.pos)

proc parseVarDecl(parser: var Parser, isPublic: bool, isReadOnly: bool = false): Stmt =
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
      none(Expr)
  return newVarDeclStmt(
    identifier, typeAnnotation, initializer, isPublic, isReadOnly, token.pos
  )

proc parseFunDecl(parser: var Parser, isPublic: bool): Stmt =
  ## Parse a function declaration
  let token = parser.peek(-1).get()
  let idTok = parser.consume({TKIdent}, "Expect identifier after 'fun'.")
  let identifier = idTok.lexeme
  let identifierPos = idTok.pos
  discard parser.consume({TKLParen}, "Expect '(' after function name.")
  parser.cleanUpNewLines()
  var parameters: seq[FunctionParam]
  while not parser.check({TKRParen}):
    if parameters.len > 0:
      discard parser.consume({TKComma}, "Expect ',' between parameters.")
      parser.cleanUpNewLines()
    let nameTok = parser.consume({TKIdent}, "Expect parameter name.")
    let paramName = nameTok.lexeme
    let namePos = nameTok.pos
    discard parser.consume({TKColon}, "Expect ':' after parameter name.")
    let typeStartTok = parser.peek().get()
    let paramType = parser.parseType()
    let typePos = typeStartTok.pos
    parameters.add(
      FunctionParam(
        name: paramName, namePos: namePos, paramType: paramType, paramTypePos: typePos
      )
    )
    parser.cleanUpNewLines()
  discard parser.consume(
    {TKRParen},
    "Expect ')' after function parameters but got " & $parser.peek().get().kind,
  )
  let (returnType, returnTypePos) =
    if parser.match({TKColon}):
      let typeTok = parser.peek().get()
      (parser.parseType(), typeTok.pos)
    else:
      (Type(kind: TkPrimitive, primitive: Void), token.pos)
  let body =
    if parser.check({TKLBrace}):
      some(parser.parseStatement())
    else:
      none(Stmt)
  var builder = newFunDeclBuilder(
    identifier, identifierPos, returnType, returnTypePos, isPublic, token.pos
  )
  for param in parameters:
    addParameter(builder, param)
  if body.isSome:
    setBody(builder, body.get())
  return buildFunDeclStmt(builder)

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

  while true:
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

proc parseIfStmt(parser: var Parser): Stmt =
  ## Parse an if statement with optional elif and else branches
  let ifToken = parser.peek(-1).get() # 'if' token already matched

  discard parser.consume({TKLParen}, "Expect '(' after 'if'")
  let ifCond = parser.parseExpression()
  discard parser.consume({TKRParen}, "Expect ')' after if condition")
  parser.cleanUpNewLines()
  discard
    parser.consume({TKLBrace}, "Expect '{' after if condition for block statement.")
  let ifBody = parser.parseBlockStmt()
  parser.cleanUpNewLines()
  var builder = newIfStmtBuilder(ifToken.pos)
  addIfBranch(builder, randomID(), ifCond, ifBody)

  while parser.match({TKElif}):
    let elifToken = parser.peek(-1).get()
    discard parser.consume({TKLParen}, "Expect '(' after 'elif'")
    let elifCond = parser.parseExpression()
    discard parser.consume({TKRParen}, "Expect ')' after elif condition")
    parser.cleanUpNewLines()
    discard
      parser.consume({TKLBrace}, "Expect '{' after elif condition for block statement.")
    let elifBody = parser.parseBlockStmt()
    parser.cleanUpNewLines()
    addIfBranch(builder, randomID(), elifCond, elifBody)

  if parser.match({TKElse}):
    discard parser.consume({TKLBrace}, "Expect '{' after else for block statement.")
    let elseBody = parser.parseBlockStmt()
    setElseBranch(builder, randomID(), elseBody)

  return buildIfStmt(builder)

proc parseWhileStmt(parser: var Parser): Stmt =
  ## Parse a while statement
  let whileToken = parser.peek(-1).get() # 'while' token already matched

  discard parser.consume({TKLParen}, "Expect '(' after 'while'")
  let condition = parser.parseExpression()
  discard parser.consume({TKRParen}, "Expect ')' after while condition")
  parser.cleanUpNewLines()
  discard
    parser.consume({TKLBrace}, "Expect '{' after while condition for block statement.")
  let body = parser.parseBlockStmt()
  return newWhileStmt(randomID(), condition, body, whileToken.pos)

proc parseStructDecl(parser: var Parser, isPublic: bool): Stmt =
  ## Parse a struct definition: struct Name { ... }
  let structTok = parser.peek(-1).get()
  let idTok = parser.consume({TKIdent}, "Expect identifier after 'struct'")
  let identifier = idTok.lexeme
  let identifierPos = idTok.pos
  discard parser.consume({TKLBrace}, "Expect '{' after struct name")
  var members: Table[string, ast.StructMember]
  parser.cleanUpNewLines()
  while not parser.check({TKRBrace}):
    # Flexible preamble for struct members: comments, annotations, newlines
    var comments: seq[string]
    var annotationsTable: Table[string, Annotation] = initTable[string, Annotation]()
    while true:
      if parser.check({TKComment}):
        let token = parser.peek().get()
        comments.add(token.lexeme)
        discard parser.advance()
      elif parser.check({TKNewline, TKSemicolon}):
        discard parser.advance()
      elif parser.check({TKHash}):
        let anns = parser.parseAnnotations()
        for k, v in anns:
          annotationsTable[k] = v
      else:
        break
    var pub = false
    if parser.match({TKPub}):
      pub = true
    let nameTok = parser.consume({TKIdent}, "Expect member name in struct")
    let memberName = nameTok.lexeme
    let memberNamePos = nameTok.pos
    discard parser.consume({TKColon}, "Expect ':' after member name in struct")
    let typeStartTok = parser.peek().get()
    let memberType = parser.parseType()
    let memberTypePos = typeStartTok.pos
    var defaultValue: Option[Expr] = none(Expr)
    if parser.match({TKEqual}):
      defaultValue = some(parser.parseExpression())
    members[memberName] = newStructMember(
      memberName,
      memberNamePos,
      memberType,
      memberTypePos,
      comments,
      Annotations(annotationsTable),
      pub,
      defaultValue,
    )
    parser.cleanUpNewLines()
    if not parser.match({TKComma}):
      break
    parser.cleanUpNewLines()
  discard parser.consume({TKRBrace}, "Expect '}' after struct members")
  # Optional semicolon/newline after struct decl
  if parser.check({TKSemicolon, TKNewline}):
    discard parser.advance()
  return newStructDeclStmt(isPublic, identifier, identifierPos, members, structTok.pos)

proc parseTypeDecl(parser: var Parser, isPublic: bool): Stmt =
  ## Parse a type definition: type Name = Type
  let typeTok = parser.peek(-1).get()
  let idTok = parser.consume({TKIdent}, "Expect identifier after 'type'")
  let identifier = idTok.lexeme
  discard parser.consume({TKEqual}, "Expect '=' after type name")
  let typeAnnotation = parser.parseType()
  return newTypeDeclStmt(isPublic, identifier, typeAnnotation, typeTok.pos)

proc parseStatement(parser: var Parser, requireNewLine: bool = true): Stmt =
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
    elif parser.match({TKType}):
      # type definition
      if parser.check({TKIdent}) and parser.peek(1).isSome and
          parser.peek(1).get().kind == TKEqual:
        result = parser.parseTypeDecl(pub)
      else:
        parser.parserError("Expect '=' after 'type'")
    elif parser.match({TokenKind.TKStruct}):
      result = parser.parseStructDecl(pub)
    elif parser.match({TKReturn}):
      result = parser.parseReturnStmt()
    elif parser.match({TKLBrace}):
      result = parser.parseBlockStmt()
    elif parser.match({TKIf}):
      result = parser.parseIfStmt()
    elif parser.match({TKWhile}):
      result = parser.parseWhileStmt()
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
    return Stmt(kind: SkNop)

#import tree

proc parseModule*(file: FileInfo): tuple[hasError: bool, module: Stmt] =
  ## Parse a module
  var parser = newParser(file)
  var statements: seq[Stmt]
  while not parser.isAtEnd():
    statements.add(parser.parseStatement())
  result.hasError = parser.hasError
  result.module = Stmt(
    kind: SkModule, moduleStmt: ModuleStmt(name: file.name, statements: statements)
  )
  #echo treeRepr(result.module)
