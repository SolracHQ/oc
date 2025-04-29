# OverC TODO List

## High Priority

- [x] **Code Generation Fixes**
  - [x] Fix repeated header files when multiple external functions use the same header file
  - [x] Fix weird code generation artifacts (unnecessary semicolons after newlines)
  - [x] Improve generated C code quality and readability

- [x] **Functional Compiler**

- [x] **Improve Pipeline**
  - [x] Add intermediate cAST step between language AST and code generation
  - [x] Move all text generation to code generator (no logic)
  - [x] Move all data transformation to transpiler (no text generation)

- [x] **Type System Improvements**
  - [x] Enhance type inference to resolve metatypes properly (add additional pass)
  - [x] Fix type resolution for variables that end with metatypes

- [x] **AST Construction**
  - [x] Add helper function to create AST variants to avoid uninitialized members

- [x] **Fix Infinite recursion when resolving self-referential types**
  - [x] Fix infinite recursion when resolving self-referential types (e.g., `struct Foo { next: *Foo }`)

## Medium Priority

- [ ] **Core Language Features**
  - [x] Implement structs
  - [x] Add support for pointers
  - [ ] Introduce arrays
  - [ ] Implement control flow statements
    - [x] If/else conditionals
    - [x] While loops
    - [ ] For loops
  - [ ] Design and implement generics via type erasure
  - [x] Reconsider function calling conventions
  - [x] Add optional parameters
  - [ ] Add implicit return variable
  - [ ] Add checker for every path on function to ensure that the function always returns or assigns to result
  - [ ] Consider about do-while statement

- [ ] **String Literal Support**
  - [ ] Implement string literal type
    - [ ] Define string as a struct with `len` and pointer to data
    - [ ] Add code generation for string literals
    - [ ] Add runtime support for string struct
  - [ ] Add string operations (concatenation, comparison, etc.)

- [ ] **Module System**
  - [ ] Implement import statement
  - [ ] Implement module system to support multiple files
  - [ ] Enable core module development

- [ ] **Documentation**
  - [ ] Update Formal Syntax documentation to reflect implementation changes
  - [ ] Document lessons learned from implementation phase

## Low Priority

- [ ] **Developer Experience**
  - [ ] Improve error messages with more helpful hints
  - [ ] Add better compiler diagnostics
  - [ ] Improve compilation error reporting
  - [x] Fix difference on how error is reported in every stage (currently inconsistent: sometimes msg,pos,hint; other times pos,msg,hint; and others just msg,pos)
  - [ ] Make integration tests
  - [ ] Make test for each stage
  - [ ] Improve in-code documentation (currently a mess and confusing during changes)

- [ ] **Design Decisions**
  - [ ] Decide on method call syntax (Nim-like dot syntax vs traditional)
  - [ ] Consider implementing traits or interfaces
  - [ ] Evaluate whether to implement tagged unions

## Future Considerations

- [ ] Self-hosting: Rewrite the compiler in OverC
- [ ] Standard library design and implementation
- [ ] Build system integration