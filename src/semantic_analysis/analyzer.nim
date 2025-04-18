import ../parser/parser
import ../types/[file_info, position, scope, ast]
import ../reporter

# Import our specialized analyzers
import symbol_mapper
import reachability_checker
import type_inferencer
import type_checker

type Analyzer* = ref object
  fileInfo*: FileInfo
  scope*: Scope
  module: Stmt
  hasError*: bool
  symbolMapper: SymbolMapper
  reachabilityChecker: ReachabilityChecker
  typeInferencer: TypeInferencer
  typeChecker: TypeChecker

proc analyzerError(analyzer: Analyzer, position: Position, msg: string) =
  ## Logs an error during analysis
  logError("Analyzer", msg, position)
  analyzer.hasError = true

proc newAnalyzer*(fileInfo: FileInfo): Analyzer =
  ## Creates a new analyzer for the given file
  let (hasError, module) = parseModule(fileInfo)
  result = Analyzer()
  result.hasError = hasError
  result.fileInfo = fileInfo
  result.scope = newScope(ModuleScope, nil, fileInfo.name)
  result.module = module

  # Initialize specialized analyzers
  result.symbolMapper = newSymbolMapper(fileInfo, result.scope)
  result.reachabilityChecker = newReachabilityChecker(fileInfo)
  result.typeInferencer = newTypeInferencer(fileInfo)
  result.typeChecker = newTypeChecker(fileInfo)

proc analyze*(
    analyzer: Analyzer
): tuple[hasError: bool, symbolTable: Scope, module: Stmt] =
  # Only run if parsing didn't fail
  if analyzer.hasError:
    return (analyzer.hasError, analyzer.scope, analyzer.module)

  # Stage 1: Initialize the symbol table
  initializeTable(analyzer.symbolMapper, analyzer.scope, analyzer.module)
  analyzer.hasError = analyzer.hasError or analyzer.symbolMapper.hasError

  # If symbol mapping failed, don't proceed to next stages
  if analyzer.hasError:
    return (analyzer.hasError, analyzer.scope, analyzer.module)

  # Stage 2: Analyze reachability
  analyzeReachability(analyzer.reachabilityChecker, analyzer.scope, analyzer.module)
  analyzer.hasError = analyzer.hasError or analyzer.reachabilityChecker.hasError

  # If reachability analysis failed, don't proceed to type inference
  if analyzer.hasError:
    return (analyzer.hasError, analyzer.scope, analyzer.module)

  # Stage 3: Analyze type inference
  analyzeTypeInference(analyzer.typeInferencer, analyzer.scope, analyzer.module)
  analyzer.hasError = analyzer.hasError or analyzer.typeInferencer.hasError

  # If type inference failed, don't proceed to type checking
  if analyzer.hasError:
    return (analyzer.hasError, analyzer.scope, analyzer.module)

  # Stage 4: Analyze type checking
  analyzeTypeChecking(analyzer.typeChecker, analyzer.scope, analyzer.module)
  analyzer.hasError = analyzer.hasError or analyzer.typeChecker.hasError

  # Return results
  return (analyzer.hasError, analyzer.scope, analyzer.module)
