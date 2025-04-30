import std/options
import std/tables
import std/strutils
import ../types/[types, ast, annotation]

proc annotationRepr*(anns: Annotations, indent: int = 0): string =
  ## Pretty print annotations as a tree
  let indentStr = "  annotations".repeat(indent)

proc treeRepr*(node: Expr, indent: int = 0): string

proc treeRepr*(node: Stmt, indent: int = 0): string =
  let indentStr = "  ".repeat(indent)
  if node.isNil:
    return indentStr & "<nil stmt>\n"
  if false:
    result.add annotationRepr(node.annotations, indent)
  case node.kind
  of SkModule:
    result.add indentStr & "module " & node.moduleStmt.name & ":\n"
    for s in node.moduleStmt.statements:
      result.add treeRepr(s, indent + 1)
  of SkVarDecl:
    let vd = node.varDeclStmt
    result.add indentStr & (if vd.isReadOnly: "let" else: "var") & " " & vd.identifier &
      ": " & $vd.typeAnnotation & "\n"
    if vd.initializer.isSome:
      result.add indentStr & "  init:\n"
      result.add treeRepr(vd.initializer.get, indent + 2)
  of SkFunDecl:
    let fd = node.funDeclStmt
    result.add indentStr & "func " & fd.identifier & "("
    for i, p in fd.parameters:
      if i > 0:
        result.add ", "
      result.add p.name & ": " & $p.paramType
    result.add "): " & $fd.returnType & "\n"
    if fd.body.isSome:
      result.add indentStr & "  body:\n"
      result.add treeRepr(fd.body.get, indent + 2)
    else:
      result.add indentStr & "  body: <empty>\n"
  of SkTypeDecl:
    let td = node.typeDeclStmt
    result.add indentStr & "type " & td.identifier & ": " & $td.typeAnnotation & "\n"
  of SkStructDecl:
    let sd = node.structDeclStmt
    result.add indentStr & "struct " & sd.identifier & ":\n"
    for _, m in sd.members:
      result.add indentStr & "  " & (if m.isPublic: "pub " else: "") & m.name & ": " &
        $m.memberType
      if false:
        result.add " " & annotationRepr(m.annotations, 0).strip
      result.add "\n"
      if m.defaultValue.isSome:
        result.add indentStr & "    default:\n"
        result.add treeRepr(m.defaultValue.get, indent + 3)
  of SkBlockStmt:
    result.add indentStr & "block"
    if node.blockStmt.blockId.len > 0:
      result.add " [id=" & node.blockStmt.blockId & "]"
    result.add ":\n"
    for s in node.blockStmt.statements:
      result.add treeRepr(s, indent + 1)
  of SkExprStmt:
    result.add indentStr & "expr:\n"
    result.add treeRepr(node.exprStmt.expression, indent + 1)
  of SkReturnStmt:
    result.add indentStr & "return"
    if node.returnStmt.expression.isSome:
      result.add ":\n"
      result.add treeRepr(node.returnStmt.expression.get, indent + 1)
    else:
      result.add "\n"
  of SkIfStmt:
    result.add indentStr & "if:\n"
    for b in node.ifStmt.branches:
      result.add indentStr & "  branch"
      if b.scopeId.len > 0:
        result.add " [id=" & b.scopeId & "]"
      result.add ":\n"
      result.add indentStr & "    cond:\n"
      result.add treeRepr(b.condition, indent + 3)
      result.add indentStr & "    body:\n"
      result.add treeRepr(b.body, indent + 3)
    if node.ifStmt.elseBranch.isSome:
      let eb = node.ifStmt.elseBranch.get
      result.add indentStr & "  else"
      if eb.scopeId.len > 0:
        result.add " [id=" & eb.scopeId & "]"
      result.add ":\n"
      result.add treeRepr(eb.body, indent + 2)
  of SkWhileStmt:
    result.add indentStr & "while"
    if node.whileStmt.scopeId.len > 0:
      result.add " [id=" & node.whileStmt.scopeId & "]"
    result.add ":\n"
    result.add indentStr & "  cond:\n"
    result.add treeRepr(node.whileStmt.condition, indent + 2)
    result.add indentStr & "  body:\n"
    result.add treeRepr(node.whileStmt.body, indent + 2)
  of SkNop:
    result.add indentStr & "nop\n"

proc treeRepr*(node: Expr, indent: int = 0): string =
  let indentStr = "  ".repeat(indent)
  if node.isNil:
    return indentStr & "<nil expr>\n"
  if false:
    result.add annotationRepr(node.annotations, indent)
  # Print type info for every expression
  if not node.exprType.isNil:
    result.add indentStr & "type:\n"
    result.add node.exprType.asTree(indent + 1)
  else:
    result.add indentStr & "expression has no type\n"
  case node.kind
  of EkAssignment:
    let a = node.assignmentExpr
    result.add indentStr & "assign:\n"
    result.add indentStr & "  left:\n"
    result.add treeRepr(a.left, indent + 2)
    result.add indentStr & "  value:\n"
    result.add treeRepr(a.value, indent + 2)
  of EkLogicalExpr, EkEqualityExpr, EkComparisonExpr, EkAdditiveExpr,
      EkMultiplicativeExpr:
    let b = node.binaryOpExpr
    result.add indentStr & "binary(" & $b.operator & "):\n"
    result.add indentStr & "  left:\n"
    result.add treeRepr(b.left, indent + 2)
    result.add indentStr & "  right:\n"
    result.add treeRepr(b.right, indent + 2)
  of EkUnaryExpr:
    let u = node.unaryOpExpr
    result.add indentStr & "unary(" & $u.operator & "):\n"
    result.add treeRepr(u.operand, indent + 1)
  of EkMemberAccess:
    let m = node.memberAccessExpr
    result.add indentStr & "member:\n"
    result.add indentStr & "  object:\n"
    result.add treeRepr(m.obj, indent + 2)
    result.add indentStr & "  property: " & m.member & "\n"
  of EkFunctionCall:
    let c = node.functionCallExpr
    result.add indentStr & "call:\n"
    result.add indentStr & "  callee:\n"
    result.add treeRepr(c.callee, indent + 2)
    if c.arguments.len > 0:
      result.add indentStr & "  args:\n"
      for arg in c.arguments:
        result.add treeRepr(arg, indent + 2)
  of EkIdentifier:
    result.add indentStr & "ident: " & node.identifierExpr.name & "\n"
  of EkGroupExpr:
    result.add indentStr & "group:\n"
    result.add treeRepr(node.groupExpr.expression, indent + 1)
  of EkAddressOfExpr:
    result.add indentStr & "addrof:\n"
    result.add treeRepr(node.addressOfExpr.operand, indent + 1)
  of EkDerefExpr:
    result.add indentStr & "deref:\n"
    result.add treeRepr(node.derefExpr.operand, indent + 1)
  of EkIntLiteral:
    result.add indentStr & "int: " & $node.intLiteralExpr.value & "\n"
  of EkUIntLiteral:
    result.add indentStr & "uint: " & $node.uintLiteralExpr.value & "u\n"
  of EkFloatLiteral:
    result.add indentStr & "float: " & $node.floatLiteralExpr.value & "\n"
  of EkStringLiteral:
    result.add indentStr & "string: \"" & node.stringLiteralExpr.value & "\"\n"
  of EkCStringLiteral:
    result.add indentStr & "cstring: \"" & node.cStringLiteralExpr.value & "\"\n"
  of EkCharLiteral:
    result.add indentStr & "char: '" & $node.charLiteralExpr.value & "'\n"
  of EkBoolLiteral:
    result.add indentStr & "bool: " & $node.boolLiteralExpr.value & "\n"
  of EkNilLiteral:
    result.add indentStr & "nil\n"
  of EkStructLiteral:
    let sl = node.structLiteralExpr
    result.add indentStr & "structlit " & sl.typeName & ":\n"
    for m in sl.members:
      result.add indentStr & "  " & m.name & ":\n"
      result.add treeRepr(m.value, indent + 2)
  of EkArrayAccess:
    let aa = node.arrayAccessExpr
    result.add indentStr & "arrayaccess:\n"
    result.add indentStr & "  array:\n"
    result.add treeRepr(aa.arrayExpr, indent + 2)
    result.add indentStr & "  index:\n"
    result.add treeRepr(aa.indexExpr, indent + 2)
  of EkArrayLiteral:
    let al = node.arrayLiteralExpr
    result.add indentStr & "arrayliteral:\n"
    for elem in al.elements:
      result.add treeRepr(elem, indent + 1)

proc printTree*(node: Stmt): string =
  result = treeRepr(node)
