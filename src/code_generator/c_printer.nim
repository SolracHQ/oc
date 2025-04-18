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

proc cNodeToString*(node: CNode, indent: int = 0): string =
  let ind = repeat(' ', indent)
  let lineInfo =
    if node.pos.file != nil:
      ind & "#line " & $node.pos.line & "  \"" & absPath(node.pos.file) & "\"\n"
    else:
      ""

  case node.kind
  of CnkTranslationUnit:
    var s = ""
    # Print includes
    for inc in node.translationUnitNode.includes:
      s.add(cNodeToString(CNode(kind: CnkInclude, includeNode: inc), indent))
    # Print declarations
    for decl in node.translationUnitNode.declarations:
      s.add(cNodeToString(decl, indent))
    result = lineInfo & s
  of CnkInclude:
    if node.includeNode.isSystem:
      result = lineInfo & ind & "#include " & node.includeNode.file & "\n"
    else:
      result = lineInfo & ind & "#include \"" & node.includeNode.file & "\"\n"
  of CnkDefine:
    result = lineInfo & ind & "#define " & node.defineNode.name
    if node.defineNode.value.isSome:
      result.add(" ")
      result.add(cNodeToString(node.defineNode.value.get(), 0))
    result.add("\n")
  of CnkIfNotDef:
    result = lineInfo & ind & "#ifndef " & node.ifNotDefNode.name & "\n"
    for b in node.ifNotDefNode.body:
      result.add(cNodeToString(b, indent))
    if node.ifNotDefNode.elseBody.len > 0:
      result.add(ind & "#else\n")
      for b in node.ifNotDefNode.elseBody:
        result.add(cNodeToString(b, indent))
    result.add(ind & "#endif\n")
  of CnkIfDef:
    result = lineInfo & ind & "#ifdef " & node.ifDefNode.name & "\n"
    for b in node.ifDefNode.body:
      result.add(cNodeToString(b, indent))
    if node.ifDefNode.elseBody.len > 0:
      result.add(ind & "#else\n")
      for b in node.ifDefNode.elseBody:
        result.add(cNodeToString(b, indent))
    result.add(ind & "#endif\n")
  of CnkFunctionDecl:
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
  of CnkFunctionDef:
    let fn = node.functionDefNode
    result = lineInfo & cNodeToString(fn.declaration, indent).replace(";\n", "")
    result.add(" ")
    result.add(cNodeToString(fn.body, indent))
    result.add("\n")
  of CnkVarDecl:
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
      result.add(" = " & cNodeToString(v.initializer.get(), 0))
    result.add(";\n")
  of CnkBlockStmt:
    result = ind & "{\n"
    for stmt in node.blockStmtNode.statements:
      result.add(cNodeToString(stmt, indent + 2))
    result.add(ind & "}\n")
  of CnkExprStmt:
    result = lineInfo & ind & cNodeToString(node.exprStmtNode.expression, 0) & ";\n"
  of CnkReturnStmt:
    result = lineInfo & ind & "return"
    if node.returnStmtNode.expression.isSome:
      result.add(" " & cNodeToString(node.returnStmtNode.expression.get(), 0))
    result.add(";\n")
  of CnkIfStmt:
    let ifs = node.ifStmtNode
    result = lineInfo
    for i, branch in ifs.branches:
      if i == 0:
        result.add(ind & "if (" & cNodeToString(branch.condition, 0) & ") ")
      else:
        result.add(ind & "else if (" & cNodeToString(branch.condition, 0) & ") ")
      result.add("{\n" & cNodeToString(branch.body, indent + 2) & ind & "}\n")
    if ifs.elseBranch.isSome:
      result.add(ind & "else ")
      result.add("{\n" & cNodeToString(ifs.elseBranch.get(), indent + 2) & ind & "}\n")
  of CnkAssignment:
    let a = node.assignmentNode
    result = ind & cNodeToString(a.lhs, 0) & " = " & cNodeToString(a.rhs, 0) & ";\n"
  of CnkBinaryExpr:
    let b = node.binaryExprNode
    result =
      ind & cNodeToString(b.left, 0) & " " & tokenKindToCOperator(b.operator) & " " &
      cNodeToString(b.right, 0)
  of CnkUnaryExpr:
    let u = node.unaryExprNode
    result = ind & tokenKindToCOperator(u.operator) & cNodeToString(u.operand, 0)
  of CnkFunctionCall:
    let f = node.functionCallNode
    let args = f.arguments.mapIt(cNodeToString(it, 0)).join(", ")
    result = ind & cNodeToString(f.callee, 0) & "(" & args & ")"
  of CnkIdentifier:
    result = ind & node.identifierNode.name
  of CnkArrayAccess:
    let a = node.arrayAccessNode
    result = ind & cNodeToString(a.array, 0) & "[" & cNodeToString(a.index, 0) & "]"
  of CnkGroupExpr:
    result = ind & "(" & cNodeToString(node.groupNode.expression, 0) & ")"
  of CnkAddressOf:
    result = ind & "&" & cNodeToString(node.addressOfNode.operand, 0)
  of CnkDereference:
    result = ind & "*" & cNodeToString(node.dereferenceNode.operand, 0)
  of CnkIntLiteral:
    result = ind & $node.intLiteralNode.value
  of CnkUIntLiteral:
    result = ind & $node.uintLiteralNode.value & "U"
  of CnkFloatLiteral:
    result = ind & $node.floatLiteralNode.value
  of CnkStringLiteral:
    result = ind & '"' & node.stringLiteralNode.value & '"'
  of CnkCharLiteral:
    result = ind & "'" & node.charLiteralNode.value & "'"
  of CnkBoolLiteral:
    result = ind & (if node.boolLiteralNode.value: "1" else: "0")
  of CnkNullLiteral:
    result = ind & "NULL"
  of CnkNop:
    result = ""
