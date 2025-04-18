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
                 | BlockStmt
                 | ExprStmt
                 | IfStmt

VarDecl        ::= [Annotation] "var" Identifier [":" Type] ["=" Expression] ( ";" | "\n" )
LetDecl        ::= [Annotation] "let" Identifier [":" Type] ["=" Expression] ( ";" | "\n" )
FunDecl        ::= [Annotation] "pub"? "fun" Identifier "(" Parameters ")" [":" Type] BlockStmt
BlockStmt      ::= "{" Statement* "}"
ExprStmt       ::= Expression ( ";" | "\n" )

IfStmt         ::= "if" "(" Expression ")" Statement ( "elif" "(" Expression ")" Statement )* [ "else" Statement ]

Parameters     ::= (Parameter ("," Parameter)*)? 
Parameter      ::= Identifier ":" Type
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
UnaryExpr      ::= ("-" | "!" ) UnaryExpr
                 | MemberExpr

# Member access and function calls (same precedence)
MemberExpr     ::= PrimaryExpr
                 | MemberExpr "." Identifier
                 | MemberExpr "(" Arguments ")"

# Highest precedence
PrimaryExpr    ::= Identifier
                 | Literal
                 | GroupExpr

GroupExpr      ::= "(" Expression ")"

Arguments      ::= (Expression ("," Expression)*)?
```

## Operators

```bnf
BinaryOp       ::= "+" | "-" | "*" | "/" | "%"
                 | "==" | "!=" | "<" | ">" | "<=" | ">="
                 | "and" | "or"

UnaryOp        ::= "-" | "!"
```

## Literals

```bnf
Literal        ::= IntLiteral
                 | FloatLiteral
                 | StringLiteral
                 | CharLiteral
                 | BoolLiteral
                 | "nil"

BoolLiteral    ::= "true" | "false"
```

## Types

```bnf
Type           ::= PrimitiveType
                 | Identifier  # User-defined type

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
