import ../types/[position, file_info, token]
import ../reporter
import std/strutils

type Lexer* = object
  pos*: Position
  start*: int
  tokens*: seq[Token]
  hasError*: bool

proc source(lexer: var Lexer): string =
  ## Get the source code from the file
  result = lexer.pos.file.content

proc current(lexer: var Lexer): var int {.inline.} =
  ## Get the current character in the source code
  result = lexer.pos.offset

proc lexerError*(lexer: var Lexer, error: string, hint: string = "") =
  ## Log an error during lexing
  logError("Lexer", lexer.pos, error, hint)
  lexer.hasError = true

proc newLexer*(file: FileInfo): Lexer =
  ## Create a new lexer for the given file
  result.pos = Position(line: 1, column: 1, offset: 0, file: file)
  result.start = 0
  result.tokens = @[]
  result.hasError = false

proc isAtEnd(lexer: var Lexer): bool {.inline.} =
  ## Check if the lexer has reached the end of the source code
  result = lexer.current >= lexer.pos.file.content.len

proc advance(lexer: var Lexer): char =
  ## Advance the lexer to the next character
  result = lexer.source()[lexer.current]
  lexer.current.inc()
  if result == '\n':
    lexer.pos.line.inc()
    lexer.pos.column = 1
  else:
    lexer.pos.column.inc()

proc peek(lexer: var Lexer, n: int = 0): char {.inline.} =
  ## Peek at the next character without advancing the lexer
  if lexer.current + n >= lexer.source().len:
    return '\0'
  return lexer.source()[lexer.current + n]

proc match(lexer: var Lexer, expected: char): bool =
  ## Match the current character with the expected character
  if isAtEnd(lexer):
    return false
  if lexer.source()[lexer.current] != expected:
    return false
  discard lexer.advance()
  return true

proc addToken(
    lexer: var Lexer,
    tokenType: TokenKind,
    text = lexer.source()[lexer.start ..< lexer.current],
) =
  ## Add a token to the lexer
  let token = Token(kind: tokenType, lexeme: text, pos: lexer.pos)
  lexer.tokens.add(token)
  lexer.start = lexer.current

proc scanString(lexer: var Lexer) =
  ## Scan a string literal
  while not isAtEnd(lexer) and (peek(lexer) != '"'):
    discard lexer.advance()
  if isAtEnd(lexer):
    lexerError(lexer, "Unterminated string literal")
    return
  discard lexer.advance() # Consume the closing "
  let stringValue = lexer.source()[lexer.start + 1 ..< lexer.current - 1]
  addToken(lexer, TkStringLit, stringValue)
  lexer.tokens[^1].stringValue = stringValue

proc scanNumber(lexer: var Lexer) =
  ## Scan a number literal
  while lexer.peek().isDigit():
    discard lexer.advance()

  var isFloat = false
  if lexer.peek() == '.' and lexer.peek(1).isDigit():
    isFloat = true
    discard lexer.advance() # Consume the '.'
    while lexer.peek().isDigit():
      discard lexer.advance()

  if lexer.peek == 'e' or lexer.peek == 'E':
    isFloat = true
    discard lexer.advance() # Consume the 'e'
    if lexer.peek() == '+' or lexer.peek() == '-':
      discard lexer.advance() # Consume the '+' or '-'
    if not lexer.peek().isDigit():
      lexerError(lexer, "Invalid scientific notation: exponent has no digits")
      return
    while lexer.peek().isDigit():
      discard lexer.advance()

  let numberValue = lexer.source()[lexer.start ..< lexer.current]

  # Handle unsigned integer literals with 'u' suffix
  if not isFloat and lexer.peek() == 'u':
    discard lexer.advance() # Consume the 'u'
    lexer.addToken(TkUIntLit)
    lexer.tokens[^1].uintValue = cast[uint](parseInt(numberValue))
  elif isFloat:
    lexer.addToken(TkFloatLit)
    lexer.tokens[^1].floatValue = parseFloat(numberValue)
  else:
    lexer.addToken(TkIntLit)
    lexer.tokens[^1].intValue = parseInt(numberValue)

proc isValidIdentChar(c: char): bool =
  ## Check if a character is valid in an identifier
  return c in {'a' .. 'z'} or c in {'A' .. 'Z'} or (c == '_') or c.isDigit()

proc scanIdentifier(lexer: var Lexer) =
  ## Scan an identifier
  while isValidIdentChar(lexer.peek()):
    discard lexer.advance()
  let ident = lexer.source()[lexer.start ..< lexer.current]
  addToken(lexer, ident.getIdentKind, ident)
  if ident.getIdentKind in Primitives:
    lexer.tokens[^1].`type` = ident.getIdentKind.toType

proc scanComment(lexer: var Lexer) =
  ## Scan a comment
  while not isAtEnd(lexer) and (lexer.peek() != '\n'):
    discard lexer.advance()
  discard lexer.advance() # Consume the newline
  addToken(lexer, TkComment)

proc scanBlockComment(lexer: var Lexer) =
  ## Scan a block comment
  var depth = 1
  while not isAtEnd(lexer):
    if lexer.peek() == '/' and lexer.peek(1) == '*':
      depth.inc()
      discard lexer.advance()
      discard lexer.advance()
    elif lexer.peek() == '*' and lexer.peek(1) == '/':
      depth.dec()
      discard lexer.advance()
      discard lexer.advance()
      if depth == 0:
        break
    else:
      discard lexer.advance()
  if depth > 0:
    lexerError(lexer, "Unterminated block comment")
    return
  addToken(lexer, TkComment)

proc scanChar(lexer: var Lexer) =
  ## Scan a character literal
  var value = ""

  if lexer.peek() == '\\':
    discard lexer.advance() # Consume the '\'
    if lexer.isAtEnd():
      lexerError(lexer, "Unterminated character literal")
      return
    let escaped = lexer.advance()
    case escaped
    of 'n':
      value = "\n"
    of 't':
      value = "\t"
    of 'r':
      value = "\r"
    of '"':
      value = "\""
    of '\'':
      value = "'"
    of '\\':
      value = "\\"
    else:
      lexerError(lexer, "Invalid escape sequence")
  elif lexer.peek() == '\'':
    lexerError(lexer, "Empty character literal")
    return
  else:
    value = $lexer.advance()

  if lexer.peek() != '\'':
    lexerError(lexer, "Unterminated character literal")
    return
  discard lexer.advance() # Consume the closing '

  lexer.addToken(TkCharLit, value)
  lexer.tokens[^1].charValue = value[0]

proc scanToken(lexer: var Lexer) =
  ## Scan a single token
  let c = lexer.advance()
  case c
  of '"':
    scanString(lexer)
  of '0' .. '9':
    scanNumber(lexer)
  of '_':
    # The only valid identifier starting with '_' is '_'
    if lexer.peek.isValidIdentChar():
      lexerError(lexer, "Invalid identifier starting with '_'")
      return
    scanIdentifier(lexer)
  of 'a' .. 'z', 'A' .. 'Z':
    # Check if it's a C-string (c followed by a double quote)
    if c == 'c' and lexer.peek() == '"':
      lexer.start = lexer.current # Reset start to after the 'c'
      discard lexer.advance() # Consume the '"'
      scanString(lexer)
      # Override the token type to CStringLit
      let old = lexer.tokens[^1]
      lexer.tokens[^1] = Token(
        kind: TkCStringLit,
        lexeme: old.lexeme,
        pos: old.pos,
        stringValue: old.stringValue,
      )
    elif c == 'r' and lexer.peek() == 'o' and lexer.peek(1) == '*':
      discard lexer.advance() # Consume the 'o'
      discard lexer.advance() # Consume the '*'
      addToken(lexer, TkROPointer)
    else:
      scanIdentifier(lexer)
  of '/':
    if lexer.peek() == '/':
      scanComment(lexer)
    elif lexer.peek() == '*':
      scanBlockComment(lexer)
    else:
      addToken(lexer, TkSlash)
  of '\'':
    scanChar(lexer)
  of '+':
    addToken(lexer, TkPlus)
  of '-':
    addToken(lexer, TkMinus)
  of '*':
    addToken(lexer, TkStar)
  of '%':
    addToken(lexer, TkPercent)
  of '&':
    addToken(lexer, TkAmpersand)
  of '=':
    if lexer.match('='):
      addToken(lexer, TkEqualEqual)
    else:
      addToken(lexer, TkEqual)
  of '!':
    if lexer.match('='):
      addToken(lexer, TkBangEqual)
    else:
      addToken(lexer, TkBang)
  of '>':
    if lexer.match('='):
      addToken(lexer, TkBiggerEqual)
    else:
      addToken(lexer, TkRAngle)
  of '<':
    if lexer.match('='):
      addToken(lexer, TkSmallerEqual)
    else:
      addToken(lexer, TkLAngle)
  of '.':
    addToken(lexer, TkDot)
  of ',':
    addToken(lexer, TkComma)
  of ':':
    addToken(lexer, TkColon)
  of ';':
    addToken(lexer, TkSemiColon)
  of '(':
    addToken(lexer, TkLParen)
  of ')':
    addToken(lexer, TkRParen)
  of '[':
    addToken(lexer, TkLBracket)
  of ']':
    addToken(lexer, TkRBracket)
  of '{':
    addToken(lexer, TkLBrace)
  of '}':
    addToken(lexer, TkRBrace)
  of '#':
    addToken(lexer, TkHash)
  of '\n':
    addToken(lexer, TkNewLine)
  of '\r', '\t', ' ':
    # Ignore whitespace
    discard
  else:
    lexerError(lexer, "Unexpected character: " & c)
    discard

proc scan*(lexer: var Lexer): tuple[hasError: bool, tokens: seq[Token]] =
  ## Scan the entire source code
  while not lexer.isAtEnd():
    lexer.start = lexer.current
    scanToken(lexer)
  result.hasError = lexer.hasError
  result.tokens = lexer.tokens
