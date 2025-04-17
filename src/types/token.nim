import position
import types
import std/tables

type 
  TokenKind* = enum
    # Delimiters
    TkNewLine ## New line
    TkDot ## .
    TkComma ## ,
    TkColon ## :
    TkSemiColon ## ;
    TkLParen ## (
    TkRParen ## )
    TkLBracket ## [
    TkRBracket ## ]
    TkLBrace ## {
    TkRBrace ## }
    TkLAngle ## <
    TkRAngle ## >
    TkHash ## #

    # Primitive types
    TkInt ## Integer
    TkInt8
    TkInt16
    TkInt32
    TkInt64
    TkUInt ## Unsigned Integer
    TkUInt8
    TkUInt16
    TkUInt32
    TkUInt64
    TkFloat ## Float
    TkFloat32
    TkFloat64
    TkString ## String
    TkChar ## Char
    TkBool ## Boolean
    TkVoid ## Void
    TkType ## Type
    TkCString ## C-compatible string
    TkCVarArgs ## C-style variable arguments
    TkVarArgs ## OverC-style variable arguments

    # Identifiers and Literals
    TkIdent ## Identifier
    TkIntLit ## Integer literal
    TkUIntLit ## Unsigned Integer literal
    TkFloatLit ## Float literal
    TkStringLit ## String literal
    TkCStringLit ## C-String literal
    TkCharLit ## Char literal
    TkBoolLit ## Boolean literal

    # Keywords
    TkVar ## var
    TkLet ## let
    TkFun ## fun
    TkConst ## const
    TkReturn ## return
    TkTrue ## true
    TkFalse ## false
    TkNil ## nil
    TkOr ## or
    TkAnd ## and
    TkPub ## pub

    # Symbols
    TkPlus ## +
    TkMinus ## -
    TkStar ## *
    TkSlash ## /
    TkPercent ## %
    TkEqual ## =
    TkBang ## !
    TkAmpersand ## &

    # Composed symbols
    TkEqualEqual ## ==
    TkBangEqual ## !=
    TkBiggerEqual ## >=
    TkSmallerEqual ## <=

    # Others
    TkComment ## Comment

  Token* = object
    lexeme*: string
    pos*: Position
    case kind*: TokenKind
    of TkIdent:
      name*: string
    of TkIntLit:
      intValue*: int
    of TkUIntLit:
      uintValue*: uint
    of TkFloatLit:
      floatValue*: float
    of TkStringLit, TkCStringLit:
      stringValue*: string
    of TkCharLit:
      charValue*: char
    of TkBoolLit:
      boolValue*: bool
    of TkInt, TkInt8, TkInt16, TkInt32, TkInt64,
       TkUInt, TkUInt8, TkUInt16, TkUInt32, TkUInt64,
       TkFloat, TkFloat32, TkFloat64,
       TkString, TkChar, TkBool,
       TkVoid, TkType, TkCString, TkCVarArgs:
      `type`*: Type
    else: 
      discard

proc newToken*(kind: TokenKind, lexeme: string, pos: Position): Token =
  result = Token(kind: kind, lexeme: lexeme, pos: pos)

const Keywords* = {
  "var": TkVar,
  "let": TkLet,
  "fun": TkFun,
  "const": TkConst,
  "return": TkReturn,
  "true": TkTrue,
  "false": TkFalse,
  "nil": TkNil,
  "or": TkOr,
  "and": TkAnd,
  "pub": TkPub,

  # Primitive types
  "Int": TkInt,
  "Int8": TkInt8,
  "Int16": TkInt16,
  "Int32": TkInt32,
  "Int64": TkInt64,
  "UInt": TkUInt,
  "UInt8": TkUInt8,
  "UInt16": TkUInt16,
  "UInt32": TkUInt32,
  "UInt64": TkUInt64,
  "Float": TkFloat,
  "Float32": TkFloat32,
  "Float64": TkFloat64,
  "String": TkString,
  "Char": TkChar,
  "Bool": TkBool,
  "Void": TkVoid,
  "CString": TkCString,
  # Meta types
  "CVarArgs": TkCVarArgs,
  "VarArgs": TkVarArgs,
  "Type": TkType,
}.toTable()

const Primitives* = { TkInt, TkInt8, TkInt16, TkInt32, TkInt64,
                     TkUInt, TkUInt8, TkUInt16, TkUInt32, TkUInt64,
                     TkFloat, TkFloat32, TkFloat64,
                     TkString, TkChar, TkBool,
                     TkVoid, TkCString, TkCVarArgs }

proc getIdentKind*(lexeme: string): TokenKind =
  if Keywords.hasKey(lexeme):
    return Keywords[lexeme]
  else:
    return TkIdent

proc `$`*(kind: TokenKind): string =
  ## Converts a token kind to a string representation
  case kind:
    of TkNewLine: "'\\n'"
    of TkDot: "'.'"
    of TkComma: "','"
    of TkColon: "':'"
    of TkSemiColon: "';'"
    of TkLParen: "'('"
    of TkRParen: "')'"
    of TkLBracket: "'['"
    of TkRBracket: "']'"
    of TkLBrace: "'{'"
    of TkRBrace: "'}''"
    of TkLAngle: "'<'"
    of TkRAngle: "'>''"
    of TkAmpersand: "'&'"
    of TkHash: "'#'"
    of TkInt: "'Int'"
    of TkInt8: "'Int8'"
    of TkInt16: "'Int16'"
    of TkInt32: "'Int32'"
    of TkInt64: "'Int64'"
    of TkUInt: "'UInt'"
    of TkUInt8: "'UInt8'"
    of TkUInt16: "'UInt16'"
    of TkUInt32: "'UInt32'"
    of TkUInt64: "'UInt64'"
    of TkFloat: "'Float'"
    of TkFloat32: "'Float32'"
    of TkFloat64: "'Float64'"
    of TkString: "'String'"
    of TkChar: "'Char'" 
    of TkBool: "'Bool'"
    of TkVoid: "'Void'"
    of TkType: "'Type'"
    of TkCString: "'CString'"
    of TkCVarArgs: "'CVarArgs'"
    of TkVarArgs: "'VarArgs'"
    of TkIdent: "'Ident'"
    of TkIntLit: "'IntLit'"
    of TkUIntLit: "'UIntLit'"
    of TkFloatLit: "'FloatLit'"
    of TkStringLit: "'StringLit'"
    of TkCStringLit: "'CStringLit'"
    of TkCharLit: "'CharLit'"
    of TkBoolLit: "'BoolLit'"
    of TkVar: "'var'"
    of TkLet: "'let'"
    of TkFun: "'fun'"
    of TkConst: "'const'"
    of TkReturn: "'return'"
    of TkTrue: "'true'"
    of TkFalse: "'false'"
    of TkNil: "'nil'"
    of TkOr: "'or'"
    of TkAnd: "'and'"
    of TkPub: "'pub'"
    of TkPlus: "'+'"
    of TkMinus: "'-'"
    of TkStar: "'*'"
    of TkSlash: "'/'"
    of TkPercent: "'%'"
    of TkEqual: "'='"
    of TkBang: "'!'"
    of TkEqualEqual: "'=='"
    of TkBangEqual: "'!='"
    of TkBiggerEqual: "'>='"
    of TkSmallerEqual: "'<='"
    of TkComment: "'//..'"

proc toType*(kind: TokenKind): Type =
  ## Converts a token kind to a Type
  case kind:
  # Integer types
  of TkInt:
    result = Type(kind: TkPrimitive, primitive: Primitive.Int)
  of TkInt8:
    result = Type(kind: TkPrimitive, primitive: Primitive.Int8)
  of TkInt16:
    result = Type(kind: TkPrimitive, primitive: Primitive.Int16)
  of TkInt32:
    result = Type(kind: TkPrimitive, primitive: Primitive.Int32)
  of TkInt64:
    result = Type(kind: TkPrimitive, primitive: Primitive.Int64)
  
  # Unsigned integer types
  of TkUInt:
    result = Type(kind: TkPrimitive, primitive: Primitive.UInt)
  of TkUInt8:
    result = Type(kind: TkPrimitive, primitive: Primitive.UInt8)
  of TkUInt16:
    result = Type(kind: TkPrimitive, primitive: Primitive.UInt16)
  of TkUInt32:
    result = Type(kind: TkPrimitive, primitive: Primitive.UInt32)
  of TkUInt64:
    result = Type(kind: TkPrimitive, primitive: Primitive.UInt64)
  
  # Float types
  of TkFloat:
    result = Type(kind: TkPrimitive, primitive: Primitive.Float)
  of TkFloat32:
    result = Type(kind: TkPrimitive, primitive: Primitive.Float32)
  of TkFloat64:
    result = Type(kind: TkPrimitive, primitive: Primitive.Float64)
  
  # Other primitive types
  of TkBool:
    result = Type(kind: TkPrimitive, primitive: Primitive.Bool)
  of TkChar:
    result = Type(kind: TkPrimitive, primitive: Primitive.Char)
  of TkString:
    result = Type(kind: TkPrimitive, primitive: Primitive.String)
  of TkVoid:
    result = Type(kind: TkPrimitive, primitive: Primitive.Void)
  of TkCString:
    result = Type(kind: TkPrimitive, primitive: Primitive.CString)
    
  # Meta types
  of TkVarArgs:
    result = Type(kind: TkMeta, metaKind: MetaKind.MkVarArgs)
  of TkCVarArgs:
    result = Type(kind: TkMeta, metaKind: MetaKind.MkCVarArgs)
  of TkType:
    result = Type(kind: TkMeta, metaKind: MetaKind.MkType, typeRepr: (ref Type)(kind: TkMeta, metaKind: MetaKind.MkToInfer))
  else:
    # This branch should never be reached if the function is used correctly
    raise newException(ValueError, "Cannot convert TokenKind " & $kind & " to Type")

proc `$`*(token: Token): string =
  ## Converts a token to a string representation
  let lexeme = if token.kind == TkNewLine:
    "'\\n'"
  elif token.kind == TkComment: "'//..'"
  else:
    token.lexeme
  
  if token.kind in {TkInt, TkInt8, TkInt16, TkInt32, TkInt64,
                   TkUInt, TkUInt8, TkUInt16, TkUInt32, TkUInt64,
                   TkFloat, TkFloat32, TkFloat64,
                   TkString, TkChar, TkBool,
                   TkVoid, TkType, TkCString, TkCVarArgs, TkVarArgs}:
    result = "Token(" & lexeme & ", " & $token.kind & ", Type: " & $token.`type` & ")"
  else:
    result = "Token(" & lexeme & ", " & $token.kind & ")"