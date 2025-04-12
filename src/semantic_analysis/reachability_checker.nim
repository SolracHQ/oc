import ../types/[file_info, position, scope, ast]
import ../reporter
import std/options
import std/tables

type ReachabilityChecker* = ref object
  fileInfo*: FileInfo
  hasError*: bool

proc reachabilityError*(checker: ReachabilityChecker, position: Position, msg: string) =
  ## Prints an error message for the reachability checker
  logError("ReachabilityChecker", msg, position, checker.fileInfo.content)
  checker.hasError = true

proc newReachabilityChecker*(fileInfo: FileInfo): ReachabilityChecker =
  ## Creates a new reachability checker for the given file
  result = ReachabilityChecker()
  result.fileInfo = fileInfo
  result.hasError = false

proc analyzeReachability*(checker: ReachabilityChecker, scope: Scope, node: Node) =
  ## Check if variables are properly initialized before use and 
  ## that read-only variables (let) are assigned only once
  case node.kind:
  of NkModule:
    # Process each statement in the module
    for stmt in node.moduleNode.statements:
      analyzeReachability(checker, scope, stmt)
  
  of NkVarDecl:
    # Check initializer if present
    if node.varDeclNode.initializer.isSome:
      analyzeReachability(checker, scope, node.varDeclNode.initializer.get)
      
  of NkLetDecl:
    # Check initializer if present
    if node.letDeclNode.initializer.isSome:
      analyzeReachability(checker, scope, node.letDeclNode.initializer.get)
    
  of NkFunDecl:
    # Check function body if present
    let funDecl = node.funDeclNode
    if funDecl.body.isSome:
      # Use function scope for analyzing the body
      if scope.children.hasKey(funDecl.identifier):
        let functionScope = scope.children[funDecl.identifier]
        analyzeReachability(checker, functionScope, funDecl.body.get)
  
  of NkBlockStmt:
    # Process statements in block scope
    let blockScope = scope.children[node.blockStmtNode.blockId]
    for stmt in node.blockStmtNode.statements:
      analyzeReachability(checker, blockScope, stmt)
  
  of NkExprStmt:
    analyzeReachability(checker, scope, node.exprStmtNode.expression)
    
  of NkReturnStmt:
    if node.returnStmtNode.expression.isSome:
      analyzeReachability(checker, scope, node.returnStmtNode.expression.get)
      
  of NkAssignment:
    # Analyze the right side (value) first
    analyzeReachability(checker, scope, node.assignmentNode.value)
    
    # Check if variable exists and can be assigned
    let identifier = node.assignmentNode.identifier
    let symbolOpt = scope.findSymbol(identifier, node.pos)
    
    if symbolOpt.isNone:
      checker.reachabilityError(node.pos, "Cannot assign to undeclared variable '" & identifier & "'")
    else:
      let symbol = symbolOpt.get()
      
      if symbol.kind != Variable:
        checker.reachabilityError(node.pos, "Cannot assign to non-variable '" & identifier & "'")
      elif symbol.isReadOnly and symbol.isInitialized:
        # Error if trying to assign to readonly variable that's already initialized
        checker.reachabilityError(node.pos, "Cannot reassign to readonly variable '" & identifier & "'")
      else:
        # Mark the variable as initialized after successful assignment
        symbol.isInitialized = true
  
  of NkIdentifier:
    # Verify variable is initialized before use
    let identifier = node.identifierNode.name
    let symbolOpt = scope.findSymbol(identifier, node.pos)
    
    if symbolOpt.isNone:
      checker.reachabilityError(node.pos, "Undeclared identifier '" & identifier & "'")
    elif symbolOpt.get().kind == Variable and not symbolOpt.get().isInitialized:
      checker.reachabilityError(node.pos, "Variable '" & identifier & "' used before initialization")
  
  of NkFunctionCall:
    # Check the callee and all arguments
    analyzeReachability(checker, scope, node.functionCallNode.callee)
    for arg in node.functionCallNode.arguments:
      analyzeReachability(checker, scope, arg)
  
  of NkLogicalExpr, NkEqualityExpr, NkComparisonExpr, NkAdditiveExpr, NkMultiplicativeExpr:
    # Check both operands of binary expressions
    analyzeReachability(checker, scope, node.binaryOpNode.left)
    analyzeReachability(checker, scope, node.binaryOpNode.right)
    
  of NkUnaryExpr:
    analyzeReachability(checker, scope, node.unaryOpNode.operand)
    
  of NkMemberAccess:
    analyzeReachability(checker, scope, node.memberAccessNode.obj)
    
  of NkGroupExpr:
    analyzeReachability(checker, scope, node.groupNode.expression)
    
  # Literal nodes don't involve variables
  of NkIntLiteral, NkUIntLiteral, NkFloatLiteral, NkStringLiteral, NkCStringLiteral, NkCharLiteral, 
     NkBoolLiteral, NkNilLiteral, NkType, NkCommentLiteral, NkNop:
    discard