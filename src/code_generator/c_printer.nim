# Printer for CNode AST to C code
import ../types/ast_c
import ../types/types_c
import ../types/token
import ../types/file_info
import std/strutils
import std/options
import std/sets
import std/sequtils

proc cTypeToString(t: CType): string =
  ## Helper to print C types
  getCString(t)

proc tokenKindToCOperator(kind: TokenKind): string =
  case kind
  of TkPlus:
    "+"
  of TkMinus:
    "-"
  of TkStar:
    "*"
  of TkSlash:
    "/"
  of TkPercent:
    "%"
  of TkEqual:
    "="
  of TkEqualEqual:
    "=="
  of TkBangEqual:
    "!="
  of TkBiggerEqual:
    ">="
  of TkSmallerEqual:
    "<="
  of TkRAngle:
    ">"
  of TkLAngle:
    "<"
  of TkAnd:
    "&&"
  of TkOr:
    "||"
  of TkBang:
    "!"
  else:
    $kind # fallback for debugging

proc cExprToString*(node: CExpr, indent: int = 0): string

proc cStmtToString*(node: CStmt, indent: int = 0): string =
  let ind = repeat(' ', indent)
  let lineInfo =
    if node.pos.file != nil:
      ind & "#line " & $node.pos.line & "  \"" & absPath(node.pos.file) & "\"\n"
    else:
      ""
  case node.kind
  of CskTranslationUnit:
    var s = ""
    for inc in node.translationUnitNode.includes:
      s.add(cStmtToString(CStmt(kind: CskInclude, includeNode: inc), indent))
    for decl in node.translationUnitNode.declarations:
      s.add(cStmtToString(decl, indent))
    result = lineInfo & s
  of CskInclude:
    if node.includeNode.isSystem:
      result = lineInfo & ind & "#include " & node.includeNode.file & "\n"
    else:
      result = lineInfo & ind & "#include \"" & node.includeNode.file & "\"\n"
  of CskDefine:
    result = lineInfo & ind & "#define " & node.defineNode.name
    if node.defineNode.value.isSome:
      result.add(" ")
      result.add(cExprToString(node.defineNode.value.get(), 0))
    result.add("\n")
  of CskIfNotDef:
    result = lineInfo & ind & "#ifndef " & node.ifNotDefNode.name & "\n"
    for b in node.ifNotDefNode.body:
      result.add(cStmtToString(b, indent))
    if node.ifNotDefNode.elseBody.len > 0:
      result.add(ind & "#else\n")
      for b in node.ifNotDefNode.elseBody:
        result.add(cStmtToString(b, indent))
    result.add(ind & "#endif\n")
  of CskIfDef:
    result = lineInfo & ind & "#ifdef " & node.ifDefNode.name & "\n"
    for b in node.ifDefNode.body:
      result.add(cStmtToString(b, indent))
    if node.ifDefNode.elseBody.len > 0:
      result.add(ind & "#else\n")
      for b in node.ifDefNode.elseBody:
        result.add(cStmtToString(b, indent))
    result.add(ind & "#endif\n")
  of CskFunctionDecl:
    let fn = node.functionDeclNode
    let params =
      fn.parameters.mapIt(cTypeToString(it.paramType) & " " & it.name).join(", ")
    var mods = ""
    if fn.isStatic:
      mods.add("static ")
    if fn.isExtern:
      mods.add("extern ")
    if fn.isInline:
      mods.add("inline ")
    result =
      lineInfo & ind & mods & cTypeToString(fn.returnType) & " " & fn.name & "(" & params &
      ");\n"
  of CskFunctionDef:
    let fn = node.functionDefNode
    result = lineInfo & cStmtToString(fn.declaration, indent).replace(";\n", "")
    result.add(" ")
    result.add(cStmtToString(fn.body, indent))
    result.add("\n")
  of CskVarDecl:
    let v = node.varDeclNode
    var mods = ""
    if v.isStatic:
      mods.add("static ")
    if v.isExtern:
      mods.add("extern ")
    if v.isVolatile:
      mods.add("volatile ")
    if v.isConst:
      mods.add("const ")
    result = lineInfo & ind & mods & cTypeToString(v.varType) & " " & v.name
    if v.initializer.isSome:
      result.add(" = " & cExprToString(v.initializer.get(), 0))
    result.add(";\n")
  of CskBlockStmt:
    result = ind & "{\n"
    for stmt in node.blockStmtNode.statements:
      result.add(cStmtToString(stmt, indent + 2))
    result.add(ind & "}\n")
  of CskExprStmt:
    result = lineInfo & ind & cExprToString(node.exprStmtNode.expression, 0) & ";\n"
  of CskReturnStmt:
    result = lineInfo & ind & "return"
    if node.returnStmtNode.expression.isSome:
      result.add(" " & cExprToString(node.returnStmtNode.expression.get(), 0))
    result.add(";\n")
  of CskIfStmt:
    let ifs = node.ifStmtNode
    result = lineInfo
    for i, branch in ifs.branches:
      if i == 0:
        result.add(ind & "if (" & cExprToString(branch.condition, 0) & ") ")
      else:
        result.add(ind & "else if (" & cExprToString(branch.condition, 0) & ") ")
      result.add("{\n" & cStmtToString(branch.body, indent + 2) & ind & "}\n")
    if ifs.elseBranch.isSome:
      result.add(ind & "else ")
      result.add("{\n" & cStmtToString(ifs.elseBranch.get(), indent + 2) & ind & "}\n")
  of CskNop:
    result = ""

proc cExprToString*(node: CExpr, indent: int = 0): string =
  let ind = repeat(' ', indent)
  case node.kind
  of CekAssignment:
    let a = node.assignmentNode
    result = ind & cExprToString(a.lhs, 0) & " = " & cExprToString(a.rhs, 0)
  of CekBinaryExpr:
    let b = node.binaryExprNode
    result =
      ind & cExprToString(b.left, 0) & " " & tokenKindToCOperator(b.operator) & " " &
      cExprToString(b.right, 0)
  of CekUnaryExpr:
    let u = node.unaryExprNode
    result = ind & tokenKindToCOperator(u.operator) & cExprToString(u.operand, 0)
  of CekFunctionCall:
    let f = node.functionCallNode
    let args = f.arguments.mapIt(cExprToString(it, 0)).join(", ")
    result = ind & cExprToString(f.callee, 0) & "(" & args & ")"
  of CekIdentifier:
    result = ind & node.identifierNode.name
  of CekArrayAccess:
    let a = node.arrayAccessNode
    result = ind & cExprToString(a.array, 0) & "[" & cExprToString(a.index, 0) & "]"
  of CekGroupExpr:
    result = ind & "(" & cExprToString(node.groupNode.expression, 0) & ")"
  of CekAddressOf:
    result = ind & "&" & cExprToString(node.addressOfNode.operand, 0)
  of CekDereference:
    result = ind & "*" & cExprToString(node.dereferenceNode.operand, 0)
  of CekIntLiteral:
    result = ind & $node.intLiteralNode.value
  of CekUIntLiteral:
    result = ind & $node.uintLiteralNode.value & "U"
  of CekFloatLiteral:
    result = ind & $node.floatLiteralNode.value
  of CekStringLiteral:
    result = ind & '"' & node.stringLiteralNode.value & '"'
  of CekCharLiteral:
    result = ind & "'" & node.charLiteralNode.value & "'"
  of CekBoolLiteral:
    result = ind & (if node.boolLiteralNode.value: "1" else: "0")
  of CekNullLiteral:
    result = ind & "NULL"
