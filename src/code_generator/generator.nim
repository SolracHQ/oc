import ../types/[scope, position, file_info, ast, types, token]
import ../reporter
import ../semantic_analysis/analyzer
import std/tables
import std/options
import std/os
import std/strutils

type
  # Represents a code generator
  CodeGenerator* = ref object
    moduleSymbols*: seq[Symbol]
    fileInfo*: file_info.FileInfo
    hasError*: bool
    entryPoint*: Option[Symbol]
    symbolTable*: Scope
    module*: Node
    includes: seq[string]

proc generatorError(codeGen: CodeGenerator, msg: string, pos: Position, hint: string = "") =
  ## Log an error during code generation
  logError("CodeGenerator", msg, pos, hint)
  codeGen.hasError = true

proc newCodeGenerator*(fileInfo: file_info.FileInfo): CodeGenerator =
  ## Creates a new code generator for the given file
  result = CodeGenerator(
    fileInfo: fileInfo,
  )
  let analyzer = newAnalyzer(fileInfo)
  if analyzer.hasError:
    result.hasError = true
    return
  
  let (hasError, scope, module) = analyzer.analyze()
  result.moduleSymbols = @[]
  result.hasError = hasError
  result.entryPoint = none(Symbol)
  result.symbolTable = scope
  result.module = module

# Helper functions to reduce repetition
proc checkType(codeGen: CodeGenerator, typeObj: Type, pos: Position, context: string): Option[string] =
  ## Checks if the type has a valid C representation and returns it
  ## If not, reports an error and returns none
  let cTypeOpt = typeObj.getCType()
  if cTypeOpt.isNone:
    codeGen.generatorError(
      "Unsupported type for " & context & ": " & $typeObj,
      pos
    )
    return none(string)
  
  return cTypeOpt

proc generateNodeExpression(codeGen: CodeGenerator, scope: Scope, node: Node, indent: int = 0): Option[string] =
  ## Generates code for an expression node
  ## Returns none if there's an error
  var code = "  ".repeat(indent)
  
  case node.kind:
  of NkIntLiteral:
    code.add($node.intLiteralNode.value)
  of NkUIntLiteral:
    code.add($node.uintLiteralNode.value & "u")
  of NkFloatLiteral:
    code.add($node.floatLiteralNode.value)
  of NkStringLiteral:
    # Create a string literal with length and pointer
    code.add("(String){.length = " & $node.stringLiteralNode.value.len & ", .ptr = \"" & 
      node.stringLiteralNode.value.replace("\"", "\\\"") & "\"}")
  of NkCStringLiteral:
    code.add("\"" & node.cStringLiteralNode.value.replace("\"", "\\\"") & "\"")
  of NkCharLiteral:
    code.add("'" & $node.charLiteralNode.value & "'")
  of NkBoolLiteral:
    code.add(if node.boolLiteralNode.value: "true" else: "false")
  of NkNilLiteral:
    code.add("NULL")
  of NkIdentifier:
    # Find the symbol in the current scope
    let symbolOpt = scope.findSymbol(node.identifierNode.name, node.pos)
    if symbolOpt.isNone:
      codeGen.generatorError("Symbol not found: " & node.identifierNode.name, node.pos)
      return none(string)
    code.add(symbolOpt.get().canonicalName)
  of NkAdditiveExpr, NkMultiplicativeExpr, NkEqualityExpr, NkComparisonExpr, NkLogicalExpr:
    # Get the operator as a string
    let opStr = case node.binaryOpNode.operator:
      of TkPlus: "+"
      of TkMinus: "-"
      of TkStar: "*"
      of TkSlash: "/"
      of TkPercent: "%"
      of TkEqualEqual: "=="
      of TkBangEqual: "!="
      of TkLAngle: "<"
      of TkSmallerEqual: "<="
      of TkRAngle: ">"
      of TkBiggerEqual: ">="
      of TkAnd: "&&"
      of TkOr: "||"
      else: ""
    
    let leftOpt = codeGen.generateNodeExpression(scope, node.binaryOpNode.left)
    if leftOpt.isNone:
      return none(string)
    
    let rightOpt = codeGen.generateNodeExpression(scope, node.binaryOpNode.right)
    if rightOpt.isNone:
      return none(string)
    
    code.add("(" & leftOpt.get() & " " & opStr & " " & rightOpt.get() & ")")
  of NkUnaryExpr:
    # Get the operator as a string
    let opStr = case node.unaryOpNode.operator:
      of TkMinus: "-"
      of TkBang: "!"
      else: ""
    
    let operandOpt = codeGen.generateNodeExpression(scope, node.unaryOpNode.operand)
    if operandOpt.isNone:
      return none(string)
    
    code.add(opStr & "(" & operandOpt.get() & ")")
  of NkFunctionCall:
    let calleeOpt = codeGen.generateNodeExpression(scope, node.functionCallNode.callee)
    if calleeOpt.isNone:
      return none(string)
    
    code.add(calleeOpt.get() & "(")
    for i, arg in node.functionCallNode.arguments:
      if i > 0:
        code.add(", ")
      
      let argOpt = codeGen.generateNodeExpression(scope, arg)
      if argOpt.isNone:
        return none(string)
      
      code.add(argOpt.get())
    
    code.add(")")
  of NkGroupExpr:
    let exprOpt = codeGen.generateNodeExpression(scope, node.groupNode.expression)
    if exprOpt.isNone:
      return none(string)
    
    code.add("(" & exprOpt.get() & ")")
  of NkAssignment:
    # Find the symbol in the current scope
    let symbolOpt = scope.findSymbol(node.assignmentNode.identifier, node.pos)
    if symbolOpt.isNone:
      codeGen.generatorError("Symbol not found: " & node.assignmentNode.identifier, node.pos)
      return none(string)
    
    # Check if the symbol is read-only
    if symbolOpt.get().isReadOnly():
      codeGen.generatorError("Cannot assign to read-only variable: " & node.assignmentNode.identifier, node.pos)
      return none(string)
    
    let valueOpt = codeGen.generateNodeExpression(scope, node.assignmentNode.value)
    if valueOpt.isNone:
      return none(string)
    
    code.add(symbolOpt.get().canonicalName & " = " & valueOpt.get())
  of NkCommentLiteral:
    # Special case for comments
    code.add(node.commentLiteralNode.value)
  else:
    codeGen.generatorError("Unsupported expression node type: " & $node.kind, node.pos)
    return none(string)
  
  return some(code)

proc generateNodeStatement(codeGen: CodeGenerator, scope: Scope, node: Node, indentation: int): Option[string] =
  ## Generates code for a statement node
  ## Returns none if there's an error
  var code = ""
  let indent = "  ".repeat(indentation)
  
  case node.kind:
  of NkVarDecl, NkLetDecl:
    # Find the symbol in the current scope
    let symbolName = if node.kind == NkVarDecl: node.varDeclNode.identifier else: node.letDeclNode.identifier
    let symbolOpt = scope.findSymbol(symbolName, node.pos)
    if symbolOpt.isNone:
      codeGen.generatorError("Symbol not found: " & symbolName, node.pos)
      return none(string)
    
    let isConst = node.kind == NkLetDecl
    let typeOpt = codeGen.checkType(symbolOpt.get().varType, node.pos, "variable")
    if typeOpt.isNone:
      return none(string)
    
    code.add(indent & (if isConst: "const " else: "") & typeOpt.get() & " " & symbolOpt.get().canonicalName)
    
    # Add initializer if present
    let initializer = if node.kind == NkVarDecl: node.varDeclNode.initializer else: node.letDeclNode.initializer
    if initializer.isSome:
      let initOpt = codeGen.generateNodeExpression(scope, initializer.get())
      if initOpt.isNone:
        return none(string)
      
      code.add(" = " & initOpt.get())
    
    code.add(";\n")
  of NkExprStmt:
    let exprOpt = codeGen.generateNodeExpression(scope, node.exprStmtNode.expression, indentation)
    if exprOpt.isNone:
      return none(string)
    
    code.add(exprOpt.get() & ";\n")
  of NkReturnStmt:
    code.add(indent & "return")
    if node.returnStmtNode.expression.isSome:
      let exprOpt = codeGen.generateNodeExpression(scope, node.returnStmtNode.expression.get())
      if exprOpt.isNone:
        return none(string)
      
      code.add(" " & exprOpt.get())
    
    code.add(";\n")
  of NkBlockStmt:
    code.add(indent & "{\n")
    
    # Create a new block scope
    let blockScope = scope.children[node.blockStmtNode.blockId]
    
    # Generate code for each statement in the block
    for stmt in node.blockStmtNode.statements:
      let stmtOpt = codeGen.generateNodeStatement(blockScope, stmt, indentation + 1)
      if stmtOpt.isNone:
        return none(string)
      
      code.add(stmtOpt.get())
    
    
    code.add(indent & "}\n")

  of NkFunDecl:
    discard # Already handled in global scope
          
  of NkCommentLiteral:
    # Special case for comments
    code.add(indent & node.commentLiteralNode.value & "\n")
  else:
    codeGen.generatorError("Unsupported statement node type: " & $node.kind, node.pos)
    return none(string)
  
  return some(code)


proc populateGlobalSymbols*(codeGen: CodeGenerator, scope: Scope) =
  ## Populates the global symbols in the code generator
  for (name, symbol) in scope.symbols.pairs:
    if symbol.kind == Variable and symbol.isGlobal or 
       symbol.kind == Function:
      codeGen.moduleSymbols.add(symbol)
    if symbol.kind == Function:
      for annotation in symbol.node.funDeclNode.annotations:
        if annotation.name == "entrypoint":
          if codeGen.entryPoint.isSome:
            codeGen.generatorError(
              "Multiple entry points found: " & codeGen.entryPoint.get.name & " and " & symbol.name,
              symbol.node.pos
            )
          else:
            codeGen.entryPoint = some(symbol)
  for (_, childScope) in scope.children.pairs:
    populateGlobalSymbols(codeGen, childScope)

proc generateFunctionSignature(codeGen: CodeGenerator, symbol: Symbol, withNames: bool = false): string =
  ## Generates the function signature for the given symbol
  let returnTypeOpt = codeGen.checkType(symbol.returnType, symbol.node.pos, "function return type")
  if returnTypeOpt.isNone:
    return ""
  
  result = returnTypeOpt.get() & " " & symbol.canonicalName & "("
  for i, paramType in symbol.paramTypes:
    if i > 0:
      result.add(", ")
    
    let paramCTypeOpt = codeGen.checkType(paramType, symbol.node.pos, "function parameter")
    if paramCTypeOpt.isNone:
      return ""
    if withNames:
      result.add(paramCTypeOpt.get() & " " & symbol.node.funDeclNode.parameters[i].identifier)
    else:
      result.add(paramCTypeOpt.get())
  result.add(")\n")

proc generateVariableDecl(codeGen: CodeGenerator, symbol: Symbol, isConst: bool = false): Option[string] =
  ## Generates a variable declaration for the given symbol
  ## Returns none if there's an error
  let varTypeOpt = codeGen.checkType(symbol.varType, symbol.node.pos, "variable")
  if varTypeOpt.isNone:
    return none(string)
  
  var decl = ""
  if isConst:
    decl = "const " & varTypeOpt.get() & " " & symbol.canonicalName
  else:
    decl = varTypeOpt.get() & " " & symbol.canonicalName
  
  return some(decl)

proc generateModuleHFile(codeGen: CodeGenerator): string =
  ## Generates the header file for the module
  let moduleName = codeGen.fileInfo.name
  result = "#ifndef " & moduleName.toUpper() & "_H\n"
  result.add("#define " & moduleName.toUpper() & "_H\n")
  result.add("#include \"core.h\"\n")
  
  for symbol in codeGen.moduleSymbols:
    if symbol.kind == Function and symbol.node.funDeclNode.isPublic:
      result.add(generateFunctionSignature(codeGen, symbol) & ";\n")
    elif symbol.kind == Variable and symbol.node.kind == NkVarDecl and symbol.node.varDeclNode.isPublic:
      let declOpt = codeGen.generateVariableDecl(symbol)
      if declOpt.isNone:
        return ""
      result.add(declOpt.get() & ";\n")
    elif symbol.kind == Variable and symbol.node.kind == NkLetDecl and symbol.node.letDeclNode.isPublic:
      let declOpt = codeGen.generateVariableDecl(symbol, true)
      if declOpt.isNone:
        return ""
      result.add(declOpt.get() & ";\n")
  
  result.add("#endif // " & moduleName.toUpper() & "_H\n")

proc generateMainFunction(codeGen: CodeGenerator): string =
  ## Generates the main function for the module if entry point is defined
  ## For now only empty main function is sported
  ## Entry points should always return Int32 (for now)
  if codeGen.entryPoint.isNone:
    return ""
  let entryPoint = codeGen.entryPoint.get()
  if entryPoint.returnType.kind != TkPrimitive or entryPoint.returnType.primitive != Int32:
    codeGen.generatorError(
      "Entry point " & entryPoint.name & " must return Int32",
      entryPoint.node.pos
    )
    return ""
  result = "int main(int argc, char** argv) {\n"
  result.add("  return " & entryPoint.canonicalName & "();\n")
  result.add("}\n")

proc generateModuleCFile(codeGen: CodeGenerator): string =
  ## Generates the C file for the module
  let moduleName = codeGen.fileInfo.name
  result = "#include \"" & moduleName & ".h\"\n\n"
  
  # Generate global symbols declarations
  for symbol in codeGen.moduleSymbols:
    if symbol.kind == Function and not symbol.node.funDeclNode.isPublic and not symbol.node.funDeclNode.annotations.hasAnnotation("external"):
      result.add(generateFunctionSignature(codeGen, symbol) & ";\n")
    elif symbol.kind == Function:
      if symbol.node.funDeclNode.annotations.hasAnnotation("external"):
        if symbol.node.funDeclNode.annotations.getAnnotationArgValue("external", "header").isSome:
          let headerName = symbol.node.funDeclNode.annotations.getAnnotationArgValue("external", "header").get()
          result.add("#include \"" & headerName.stringLiteralNode.value & "\"\n")
    elif symbol.kind == Variable and symbol.node.kind == NkVarDecl and not symbol.node.varDeclNode.isPublic:
      let declOpt = codeGen.generateVariableDecl(symbol)
      if declOpt.isNone:
        return ""
      result.add(declOpt.get() & ";\n")
    elif symbol.kind == Variable and symbol.node.kind == NkLetDecl and not symbol.node.letDeclNode.isPublic:
      let declOpt = codeGen.generateVariableDecl(symbol, true)
      if declOpt.isNone:
        return ""
      result.add(declOpt.get() & ";\n")
  
  result.add("\n")

  # Generate global symbol definitions (public or not)
  for symbol in codeGen.moduleSymbols:
    if symbol.kind == Function:
      if symbol.node.funDeclNode.body.isNone:
        if symbol.node.funDeclNode.annotations.hasAnnotation("external"):
          if not symbol.node.funDeclNode.annotations.getAnnotationArgValue("external", "name").isSome:
            codeGen.generatorError(
              "Function " & symbol.canonicalName & " has no external name",
              symbol.node.pos
            )
            return ""
          let externName = symbol.node.funDeclNode.annotations.getAnnotationArgValue("external", "name").get()
          if externName.kind != NkStringLiteral:
            codeGen.generatorError(
              "Function " & symbol.canonicalName & " has no external name",
              symbol.node.pos
            )
            return ""
          result.add("#define " & symbol.canonicalName & " " & externName.stringLiteralNode.value & "\n")
        else:
          codeGen.generatorError(
            "Function " & symbol.canonicalName & " has no body",
            symbol.node.pos
          )
          return ""
      else:
        var decl = codeGen.generateFunctionSignature(symbol, true)
        if decl.len > 0:
          result.add(decl & "{\n")
        # Generate function body
        let blockOpt = codeGen.generateNodeStatement(symbol.scope, symbol.node.funDeclNode.body.get(), 1)
        if blockOpt.isNone:
          return ""
        result.add(blockOpt.get())
        result.add("}\n")
    elif symbol.kind == Variable and symbol.node.kind == NkVarDecl and symbol.node.varDeclNode.initializer.isSome:
      let declOpt = codeGen.generateVariableDecl(symbol)
      if declOpt.isNone:
        return ""
      var decl = declOpt.get()
      result.add(decl & " = ")
      let initOpt = codeGen.generateNodeExpression(codeGen.symbolTable, symbol.node.varDeclNode.initializer.get())
      if initOpt.isNone:
        return ""
      result.add(initOpt.get())
      result.add(";\n")
    elif symbol.kind == Variable and symbol.node.kind == NkLetDecl and symbol.node.letDeclNode.initializer.isSome:
      let declOpt = codeGen.generateVariableDecl(symbol, true)
      if declOpt.isNone:
        return ""
      var decl = declOpt.get()
      result.add(decl & " = ")
      let initOpt = codeGen.generateNodeExpression(codeGen.symbolTable, symbol.node.letDeclNode.initializer.get())
      if initOpt.isNone:
        return ""
      result.add(initOpt.get())
      result.add(";\n")

  # Generate main function if entry point is defined
  let mainFunction = codeGen.generateMainFunction()
  if mainFunction.len > 0:
    result.add(mainFunction)
  

proc generateCode*(codeGen: CodeGenerator): bool =
  ## Generates code for the given scope
  result = true
  if codeGen.hasError:
    return false
  codeGen.populateGlobalSymbols(codeGen.symbolTable)
  if codeGen.hasError:
    return false
  # Create cache dir for generated files, it should be on .oc-cache
  # Also copy core.h from core directory to cache dir
  let cacheDir = getCurrentDir() / ".oc-cache"
  if not dirExists(cacheDir):
    createDir(cacheDir)
  let coreDir = getCurrentDir() / "core"
  let coreHeader = coreDir / "core.h"
  if not fileExists(coreHeader):
    return false
  let coreHeaderCache = cacheDir / "core.h"
  if not fileExists(coreHeaderCache):
    copyFile(coreHeader, coreHeaderCache)
  # Generate header file
  let headerFileName = cacheDir / (codeGen.fileInfo.name & ".h")
  let headerFile = open(headerFileName, fmWrite)
  headerFile.write(generateModuleHFile(codeGen))
  headerFile.close()
  if codeGen.hasError:
    return false
  # Generate C file
  let cFileName = cacheDir / (codeGen.fileInfo.name & ".c")
  let cFile = open(cFileName, fmWrite)
  cFile.write(generateModuleCFile(codeGen))
  cFile.close()
  if codeGen.hasError:
    return false
