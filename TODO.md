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

- [ ] **Type System Improvements**
  - [ ] Enhance type inference to resolve metatypes properly (add additional pass)
  - [ ] Fix type resolution for variables that end with metatypes
  - [ ] Determine approach for type variables/symbols

## Medium Priority

- [ ] **Core Language Features**
  - [ ] Implement structs
  - [ ] Add support for pointers
  - [ ] Introduce arrays
  - [ ] Design and implement generics via type erasure
  - [ ] Reconsider function calling conventions

- [ ] **Documentation**
  - [ ] Update Formal Syntax documentation to reflect implementation changes
  - [ ] Document lessons learned from implementation phase

## Low Priority

- [ ] **Developer Experience**
  - [ ] Improve error messages with more helpful hints
  - [ ] Add better compiler diagnostics
  - [ ] Improve compilation error reporting

- [ ] **Design Decisions**
  - [ ] Decide on method call syntax (Nim-like dot syntax vs traditional)
  - [ ] Consider implementing traits or interfaces
  - [ ] Evaluate whether to implement tagged unions

## Future Considerations

- [ ] Self-hosting: Rewrite the compiler in OverC
- [ ] Standard library design and implementation
- [ ] Build system integration