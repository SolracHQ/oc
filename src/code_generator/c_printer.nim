# Printer for CNode AST to C code
import ../types/ast_c
import ../types/types_c
import ../types/token
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
  let indentStr = repeat(' ', indent)
  let lineInfo =
    if node.pos.file != nil:
      "" #indentStr & "#line " & $node.pos.line & "  \"" & absPath(node.pos.file) & "\"\n"
    else:
      ""
  case node.kind
  of CskTranslationUnit:
    var resultStr = ""
    for includeStmt in node.translationUnitNode.includes:
      resultStr.add(cStmtToString(CStmt(kind: CskInclude, includeNode: includeStmt), indent))
    for declaration in node.translationUnitNode.declarations:
      resultStr.add(cStmtToString(declaration, indent))
    result = lineInfo & resultStr
  of CskInclude:
    if node.includeNode.isSystem:
      result = lineInfo & indentStr & "#include " & node.includeNode.file & "\n"
    else:
      result = lineInfo & indentStr & "#include \"" & node.includeNode.file & "\"\n"
  of CskDefine:
    result = lineInfo & indentStr & "#define " & node.defineNode.name
    if node.defineNode.value.isSome:
      result.add(" ")
      result.add(cExprToString(node.defineNode.value.get(), 0))
    result.add("\n")
  of CskIfNotDef:
    result = lineInfo & indentStr & "#ifndef " & node.ifNotDefNode.name & "\n"
    for bodyStmt in node.ifNotDefNode.body:
      result.add(cStmtToString(bodyStmt, indent))
    if node.ifNotDefNode.elseBody.len > 0:
      result.add(indentStr & "#else\n")
      for elseBodyStmt in node.ifNotDefNode.elseBody:
        result.add(cStmtToString(elseBodyStmt, indent))
    result.add(indentStr & "#endif\n")
  of CskIfDef:
    result = lineInfo & indentStr & "#ifdef " & node.ifDefNode.name & "\n"
    for bodyStmt in node.ifDefNode.body:
      result.add(cStmtToString(bodyStmt, indent))
    if node.ifDefNode.elseBody.len > 0:
      result.add(indentStr & "#else\n")
      for elseBodyStmt in node.ifDefNode.elseBody:
        result.add(cStmtToString(elseBodyStmt, indent))
    result.add(indentStr & "#endif\n")
  of CskWhileStmt:
    let whileStmt = node.whileStmtNode
    result = lineInfo & indentStr & "while (" & cExprToString(whileStmt.condition, 0) & ") "
    result.add("{\n" & cStmtToString(whileStmt.body, indent + 2) & indentStr & "}\n")
  of CskFunctionDecl:
    let fnDecl = node.functionDeclNode
    let paramsStr =
      fnDecl.parameters.mapIt(cTypeToString(it.paramType) & " " & it.name).join(", ")
    var modifiers = ""
    if fnDecl.isStatic:
      modifiers.add("static ")
    if fnDecl.isExtern:
      modifiers.add("extern ")
    if fnDecl.isInline:
      modifiers.add("inline ")
    result =
      lineInfo & indentStr & modifiers & cTypeToString(fnDecl.returnType) & " " & fnDecl.name & "(" & paramsStr &
      ");\n"
  of CskFunctionDef:
    let fnDef = node.functionDefNode
    result = lineInfo & cStmtToString(fnDef.declaration, indent).replace(";\n", "")
    result.add(" ")
    result.add(cStmtToString(fnDef.body, indent))
    result.add("\n")
  of CskVarDecl:
    let varDecl = node.varDeclNode
    var modifiers = ""
    if varDecl.isStatic:
      modifiers.add("static ")
    if varDecl.isExtern:
      modifiers.add("extern ")
    if varDecl.isVolatile:
      modifiers.add("volatile ")
    if varDecl.isConst:
      modifiers.add("const ")
    result = lineInfo & indentStr & modifiers & cTypeToString(varDecl.varType) & " " & varDecl.name
    if varDecl.initializer.isSome:
      result.add(" = " & cExprToString(varDecl.initializer.get(), 0))
    result.add(";\n")
  of CskBlockStmt:
    result = indentStr & "{\n"
    for statement in node.blockStmtNode.statements:
      result.add(cStmtToString(statement, indent + 2))
    result.add(indentStr & "}\n")
  of CskExprStmt:
    result = lineInfo & indentStr & cExprToString(node.exprStmtNode.expression, 0) & ";\n"
  of CskReturnStmt:
    result = lineInfo & indentStr & "return"
    if node.returnStmtNode.expression.isSome:
      result.add(" " & cExprToString(node.returnStmtNode.expression.get(), 0))
    result.add(";\n")
  of CskIfStmt:
    let ifStmt = node.ifStmtNode
    result = lineInfo
    for i, branch in ifStmt.branches:
      if i == 0:
        result.add(indentStr & "if (" & cExprToString(branch.condition, 0) & ") ")
      else:
        result.add(indentStr & "else if (" & cExprToString(branch.condition, 0) & ") ")
      result.add("{\n" & cStmtToString(branch.body, indent + 2) & indentStr & "}\n")
    if ifStmt.elseBranch.isSome:
      result.add(indentStr & "else ")
      result.add("{\n" & cStmtToString(ifStmt.elseBranch.get(), indent + 2) & indentStr & "}\n")
  of CskTypedef:
    let typedefNode = node.typedefNode
    result =
      lineInfo & indentStr & "typedef " & cTypeToString(typedefNode.baseType) & " " & typedefNode.name & ";\n"
  of CskStructDef:
    let structDef = node.structDefNode
    result = lineInfo & indentStr & "typedef struct " & structDef.name & " {\n"
    for member in structDef.members:
      result.add(indentStr & "  " & cTypeToString(member.paramType) & " " & member.name & ";\n")
    result.add(indentStr & "} " & structDef.name & ";\n")
  of CskNop:
    result = ""

proc cExprToString*(node: CExpr, indent: int = 0): string =
  let indentStr = repeat(' ', indent)
  case node.kind
  of CekAssignment:
    let assignment = node.assignmentNode
    result = indentStr & cExprToString(assignment.lhs, 0) & " = " & cExprToString(assignment.rhs, 0)
  of CekBinaryExpr:
    let binaryExpr = node.binaryExprNode
    result =
      indentStr & cExprToString(binaryExpr.left, 0) & " " & tokenKindToCOperator(binaryExpr.operator) & " " &
      cExprToString(binaryExpr.right, 0)
  of CekUnaryExpr:
    let unaryExpr = node.unaryExprNode
    result = indentStr & tokenKindToCOperator(unaryExpr.operator) & cExprToString(unaryExpr.operand, 0)
  of CekFunctionCall:
    let functionCall = node.functionCallNode
    let argsStr = functionCall.arguments.mapIt(cExprToString(it, 0)).join(", ")
    result = indentStr & cExprToString(functionCall.callee, 0) & "(" & argsStr & ")"
  of CekIdentifier:
    result = indentStr & node.identifierNode.name
  of CekArrayAccess:
    let arrayAccess = node.arrayAccessNode
    result = indentStr & cExprToString(arrayAccess.array, 0) & "[" & cExprToString(arrayAccess.index, 0) & "]"
  of CekGroupExpr:
    result = indentStr & "(" & cExprToString(node.groupNode.expression, 0) & ")"
  of CekAddressOf:
    result = indentStr & "&" & cExprToString(node.addressOfNode.operand, 0)
  of CekDereference:
    result = indentStr & "*" & cExprToString(node.dereferenceNode.operand, 0)
  of CekIntLiteral:
    result = indentStr & $node.intLiteralNode.value
  of CekUIntLiteral:
    result = indentStr & $node.uintLiteralNode.value & "U"
  of CekFloatLiteral:
    result = indentStr & $node.floatLiteralNode.value
  of CekStringLiteral:
    result = indentStr & '"' & node.stringLiteralNode.value & '"'
  of CekCharLiteral:
    result = indentStr & "'" & node.charLiteralNode.value & "'"
  of CekBoolLiteral:
    result = indentStr & (if node.boolLiteralNode.value: "1" else: "0")
  of CekStructLiteral:
    let structLiteral = node.structLiteralNode
    result = indentStr & "(" & structLiteral.typeName & ")"
    result.add("{")
    for i, member in structLiteral.members:
      if i > 0:
        result.add(", ")
      result.add("." & member.name & " = " & cExprToString(member.value, 0))
    result.add("}")
  of CekMemberAccess:
    let memberAccess = node.memberAccessNode
    result = indentStr & cExprToString(memberAccess.expr, 0) & "." & memberAccess.member
  of CekArrowMemberAccess:
    let arrowMemberAccess = node.arrowMemberAccessNode
    result = indentStr & cExprToString(arrowMemberAccess.expr, 0) & "->" & arrowMemberAccess.member
  of CekNullLiteral:
    result = indentStr & "NULL"
