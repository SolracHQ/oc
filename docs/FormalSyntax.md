# OverC Language Formal Syntax

This document describes the formal syntax of the OverC programming language.

## Program Structure

```bnf
Program        ::= Statement*
```

## Statements

```bnf
Statement      ::= VarDecl
                 | LetDecl
                 | FunDecl
                 | TypeDecl
                 | BlockStmt
                 | ExprStmt
                 | IfStmt
                 | WhileStmt
                 | ReturnStmt

VarDecl        ::= [Annotation] "var" Identifier [":" Type] ["=" Expression] ( ";" | "\n" )
LetDecl        ::= [Annotation] "let" Identifier [":" Type] ["=" Expression] ( ";" | "\n" )
FunDecl        ::= [Annotation] "pub"? "fun" Identifier "(" Parameters ")" [":" Type] BlockStmt
                 | "fun" Identifier "(" Parameters ")" [":" Type] BlockStmt   # Nested function (inside BlockStmt)
TypeDecl       ::= ("pub")? "type" Identifier "=" Type ( ";" | "\n" )
                 | ("pub")? StructDecl
BlockStmt      ::= "{" Statement* "}"
ExprStmt       ::= Expression ( ";" | "\n" )

IfStmt         ::= "if" "(" Expression ")" BlockStmt ( "elif" "(" Expression ")" BlockStmt )* [ "else" BlockStmt ]
WhileStmt      ::= "while" "(" Expression ")" BlockStmt
ReturnStmt     ::= "return" [Expression] ( ";" | "\n" )

Parameters     ::= (Parameter ("," Parameter)*)? 
Parameter      ::= ("pub")? Identifier ":" Type
```

## Structs

```bnf
StructDecl     ::= "struct" Identifier "{" Parameters "}" ( ";" | "\n" )
StructMemberList ::= (StructMember ("," StructMember)*)?
StructMember   ::= Identifier ":" Type ( "=" Expression )?
```

## Expressions

```bnf
# Expressions listed in order of decreasing precedence

Expression     ::= AssignmentExpr

# Assignment (lowest precedence)
AssignmentExpr ::= LogicalExpr
                 | Identifier "=" AssignmentExpr

# Logical operators
LogicalExpr    ::= EqualityExpr (("and" | "or") EqualityExpr)*

# Equality operators
EqualityExpr   ::= ComparisonExpr (("==" | "!=") ComparisonExpr)*

# Comparison operators
ComparisonExpr ::= AdditiveExpr (("<" | ">" | "<=" | ">=") AdditiveExpr)*

# Addition and subtraction
AdditiveExpr   ::= MultiplicativeExpr (("+" | "-") MultiplicativeExpr)*

# Multiplication, division, and modulo
MultiplicativeExpr ::= UnaryExpr (("*" | "/" | "%") UnaryExpr)*

# Unary operations
UnaryExpr      ::= ("-" | "!" | "&" | "*" ) UnaryExpr
                 | MemberExpr

# Member access and function calls (same precedence)
MemberExpr     ::= PrimaryExpr
                 | MemberExpr "." Identifier
                 | MemberExpr "(" Arguments ")"

# Highest precedence
PrimaryExpr    ::= Identifier
                 | Literal
                 | GroupExpr
                 | StructLiteral

GroupExpr      ::= "(" Expression ")"

StructLiteral  ::= Identifier "{" StructLiteralMembers "}"

StructLiteralMembers ::= (StructLiteralMember ("," StructLiteralMember)*)?
StructLiteralMember  ::= Identifier ":" Expression

Arguments      ::= (Expression ("," Expression)*)?
```

## Operators

```bnf
BinaryOp       ::= "+" | "-" | "*" | "/" | "%"
                 | "==" | "!=" | "<" | ">" | "<=" | ">="
                 | "and" | "or"

UnaryOp        ::= "-" | "!" | "&" | "*"
```

## Literals

```bnf
Literal        ::= IntLiteral
                 | FloatLiteral
                 | StringLiteral
                 | CStringLiteral
                 | CharLiteral
                 | BoolLiteral
                 | "nil"

CStringLiteral ::= "c" StringLiteral

BoolLiteral    ::= "true" | "false"
```

## Types

```bnf
Type           ::= PointerType
                 | PrimitiveType
                 | Identifier  # User-defined type

PointerType    ::= [ReadOnly] "*" Type

ReadOnly       ::= "ro"   # read-only, only for pointers

PrimitiveType  ::= "Int" | "Int8" | "Int16" | "Int32" | "Int64" | "Int128"
                 | "UInt" | "UInt8" | "UInt16" | "UInt32" | "UInt64"
                 | "Float" | "Float32" | "Float64"
                 | "String" | "Char" | "Bool" | "Void"
                 | "CString" | "CVarArgs" | "VarArgs"
```

## Annotations

```bnf
Annotation     ::= "#[" AnnotationName ("(" AnnotationArgs ")")? "]"
AnnotationName ::= Identifier
AnnotationArgs ::= AnnotationArg ("," AnnotationArg)*
AnnotationArg  ::= Identifier "=" Literal
                 | Literal
```

## Comments
```bnf
Comment        ::= "//" [^\n]* \n         # Line comment
                 | "/*" (. | \n)* "*/"    # Block comment
```
