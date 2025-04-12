import std/options
import ../types/[types, ast]
import strformat
import strutils

proc treeRepr*(node: Node, indent: int = 0): string

proc annotationRepr*(annotation: Annotation, indent: int = 0): string =
  ## Creates a string representation of an annotation in tree-like format
  let indentStr = "  ".repeat(indent)
  result = indentStr & annotation.name
  
  # If there are no arguments, add an empty line
  if annotation.args.len == 0:
    return result & "\n"
  else:
    result.add ":\n"
    
  let argIndent = "  ".repeat(indent + 2)

  # Process each argument
  for arg in annotation.args:
    if arg.value.isSome:
      # Key with value (key: value)
      result.add argIndent & arg.name & ":\n"
      result.add treeRepr(arg.value.get, indent + 3)
    else:
      # Flag (key without value)
      result.add argIndent & arg.name & "\n"

proc treeRepr*(node: Node, indent: int = 0): string =
  ## Creates a tree-like string representation of an AST node
  let indentStr = "  ".repeat(indent)
  
  case node.kind:
    of NkNop: 
      result = indentStr & "nop\n"
    of NkModule:
      result = indentStr & "module (" & node.moduleNode.name & "):\n"
      for stmt in node.moduleNode.statements:
        result.add treeRepr(stmt, indent + 1)
    
    of NkBlockStmt:
      result = indentStr & "block:\n"
      for stmt in node.blockStmtNode.statements:
        result.add treeRepr(stmt, indent + 1)
    
    of NkVarDecl:
      result = indentStr & "var:\n"
      result.add indentStr & "  ident: " & node.varDeclNode.identifier & "\n"
      result.add indentStr & "  type: " & $node.varDeclNode.typeAnnotation & "\n"
      if node.varDeclNode.initializer.isSome:
        result.add indentStr & "  init:\n"
        result.add treeRepr(node.varDeclNode.initializer.get, indent + 2)
      if node.varDeclNode.annotations.len > 0:
        result.add indentStr & "  annotations:\n"
        for ann in node.varDeclNode.annotations:
          result.add indentStr & annotationRepr(ann, indent + 1)
    
    of NkLetDecl:
      result = indentStr & "let:\n"
      result.add indentStr & "  ident: " & node.letDeclNode.identifier & "\n"
      result.add indentStr & "  type: " & $node.letDeclNode.typeAnnotation & "\n"
      if node.letDeclNode.initializer.isSome:
        result.add indentStr & "  init:\n"
        result.add treeRepr(node.letDeclNode.initializer.get, indent + 2)
      if node.letDeclNode.annotations.len > 0:
        result.add indentStr & "  annotations:\n"
        for ann in node.letDeclNode.annotations:
          result.add indentStr & annotationRepr(ann, indent + 1)
          
    of NkFunDecl:
      result = indentStr & "function:\n"
      result.add indentStr & "  name: " & node.funDeclNode.identifier & "\n"
      if node.funDeclNode.parameters.len > 0:
        result.add indentStr & "  params:\n"
        for param in node.funDeclNode.parameters:
          result.add indentStr & "    " & param.identifier & ": " & $param.paramType & "\n"
      
      result.add indentStr & "  returnType: " & $node.funDeclNode.returnType & "\n"
      if node.funDeclNode.annotations.len > 0:
        result.add indentStr & "  annotations:\n"
        for ann in node.funDeclNode.annotations:
          result.add indentStr & annotationRepr(ann, indent + 1)
      result.add indentStr & "  body:\n"
      if node.funDeclNode.body.isSome:
        result.add treeRepr(node.funDeclNode.body.get, indent + 2)
      else:
        result.add indentStr & "    <empty>\n"
          
    of NkExprStmt:
      result = indentStr & "expr:\n"
      result.add treeRepr(node.exprStmtNode.expression, indent + 1)
      
    of NkReturnStmt:
      result = indentStr & "return:\n"
      if node.returnStmtNode.expression.isSome:
        result.add treeRepr(node.returnStmtNode.expression.get, indent + 1)
      
    of NkAssignment:
      result = indentStr & "assign:\n"
      result.add indentStr & "  target: " & node.assignmentNode.identifier & "\n" 
      result.add indentStr & "  value:\n"
      result.add treeRepr(node.assignmentNode.value, indent + 2)
      
    of NkLogicalExpr, NkEqualityExpr, NkComparisonExpr, NkAdditiveExpr, NkMultiplicativeExpr:
      let opStr = $node.binaryOpNode.operator
      result = indentStr & fmt"binary({opStr}):" & "\n"
      result.add indentStr & "  left:\n"
      result.add treeRepr(node.binaryOpNode.left, indent + 2)
      result.add indentStr & "  right:\n"
      result.add treeRepr(node.binaryOpNode.right, indent + 2)

    of NkCommentLiteral:
      result = indentStr & "comment: ...\n"
      
    of NkUnaryExpr:
      result = indentStr & "unary(" & $node.unaryOpNode.operator & "):\n"
      result.add treeRepr(node.unaryOpNode.operand, indent + 1)
      
    of NkMemberAccess:
      result = indentStr & "member:\n"
      result.add indentStr & "  object:\n"
      result.add treeRepr(node.memberAccessNode.obj, indent + 2)
      result.add indentStr & "  property: " & node.memberAccessNode.member & "\n"
      
    of NkFunctionCall:
      result = indentStr & "call:\n"
      result.add indentStr & "  callee:\n"
      result.add treeRepr(node.functionCallNode.callee, indent + 2)
      if node.functionCallNode.arguments.len > 0:
        result.add indentStr & "  args:\n"
        for arg in node.functionCallNode.arguments:
          result.add treeRepr(arg, indent + 2)
          
    of NkIdentifier:
      result = indentStr & "ident: " & node.identifierNode.name & "\n"
      
    of NkGroupExpr:
      result = indentStr & "group:\n"
      result.add treeRepr(node.groupNode.expression, indent + 1)
      
    of NkIntLiteral:
      result = indentStr & "int: " & $node.intLiteralNode.value & "\n"
      
    of NkUIntLiteral:
      result = indentStr & "uint: " & $node.uintLiteralNode.value & "u\n"
      
    of NkFloatLiteral:
      result = indentStr & "float: " & $node.floatLiteralNode.value & "\n"
      
    of NkStringLiteral:
      result = indentStr & "string: \"" & node.stringLiteralNode.value & "\"\n"

    of NkCStringLiteral:
      result = indentStr & "cstring: \"" & node.cstringLiteralNode.value & "\"\n"
      
    of NkCharLiteral:
      result = indentStr & "char: '" & $node.charLiteralNode.value & "'\n"
      
    of NkBoolLiteral:
      result = indentStr & "bool: " & $node.boolLiteralNode.value & "\n"
      
    of NkNilLiteral:
      result = indentStr & "nil\n"
      
    of NkType:
      result = indentStr & "type: " & $node.typeNode & "\n"

proc printTree*(node: Node): string =
  ## Creates a string representation of the AST for printing
  result = treeRepr(node)
