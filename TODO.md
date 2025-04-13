# OverC TODO List

## High Priority

- [ ] **Code Generation Fixes**
  - [ ] Fix repeated header files when multiple external functions use the same header file
  - [ ] Fix weird code generation artifacts (unnecessary semicolons after newlines)
  - [ ] Improve generated C code quality and readability

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