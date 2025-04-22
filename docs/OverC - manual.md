# OverC Manual

## Overview

OverC is a statically-typed, C-inspired programming language that transpiles to C. It aims to provide a modern, expressive syntax while maintaining close compatibility with C's performance and ecosystem.

## Data Types

OverC supports the following primitive types:

| Type      | Description                       |
|-----------|-----------------------------------|
| `Int`     | Default integer                   |
| `Int8`    | 8-bit signed integer              |
| `Int16`   | 16-bit signed integer             |
| `Int32`   | 32-bit signed integer             |
| `Int64`   | 64-bit signed integer             |
| `UInt`    | Default unsigned integer          |
| `UInt8`   | 8-bit unsigned integer            |
| `UInt16`  | 16-bit unsigned integer           |
| `UInt32`  | 32-bit unsigned integer           |
| `UInt64`  | 64-bit unsigned integer           |
| `Float`   | Default floating-point            |
| `Float32` | 32-bit floating-point             |
| `Float64` | 64-bit floating-point             |
| `Bool`    | Boolean (`true`/`false`)          |
| `Char`    | Character                         |
| `String`  | OverC string                      |
| `CString` | C-compatible string               |
| `CVarArgs`| C variable arguments              |
| `VarArgs` | OverC variable arguments          |
| `Void`    | No return type                    |

### Pointers

- `*Type` — mutable pointer to `Type`
- `ro*Type` — read-only pointer to `Type`

### Structs

User-defined types grouping multiple fields.

## Operations and Casting Rules

- **No Implicit Casting:** Operations require operands of the same primitive type (except for pointer arithmetic and equality).
- **Pointer Arithmetic:** Allowed between pointers and integer types.
- **Equality:** Can compare any types for equality.
- **No Implicit Conversion:** You must explicitly convert types if needed (casting is not automatic).
- **Pointer Rules:** See below for address-of and pointer assignment.

## Variable Declarations

- **Mutable:**  
  ```overc
  var x: Int = 10
  ```
- **Immutable:**  
  ```overc
  let y: Float = 3.14
  ```

## Function Declarations

```overc
fun add(a: Int, b: Int): Int {
  return a + b
}
```

- Use `pub` for public functions:
  ```overc
  pub fun greet(name: String) { ... }
  ```

## Type and Struct Declarations

- **Type Alias:**
  ```overc
  type MyInt = Int32
  ```
- **Struct:**
  ```overc
  struct Point {
    x: Int32,
    y: Int32
  }
  ```

## Pointer and Read-Only Pointer Semantics

- `&` on a `let` variable or member yields a `ro*Type`.
- `&` on a `var` yields a `*Type`.
- Assigning to a variable of type `ro*Type` always results in a `ro*Type`, regardless of the source.

Example:
```overc
var x = 0
var px: ro*Int = &x
```

## Function Parameter Pointer Rules

- Parameter of type `*Type`: accepts `*Type`, `ro*Type`, or `nil`.
- Parameter of type `ro*Type`: accepts only `ro*Type` or `nil`.

## Entry Point

Mark the program's entry point with the `#[entrypoint]` annotation:

```overc
#[entrypoint]
fun main(): Int32 {
  // ...
  return 0
}
```

## Importing C Functions

Use the `#[include]` annotation to bind C functions:

```overc
#[include(from="<stdio.h>", name="printf")]
fun printf(format: CString, _: CVarArgs)
```

## Example

```overc
#[include(from="<stdio.h>", name="printf")]
fun printf(format: CString, _: CVarArgs)

#[entrypoint]
fun main(): Int32 {
  let msg: String = "Hello, OverC!"
  printf(c"%s\n", msg)
  return 0
}
```

## Further Reading

- [Formal Syntax](FormalSyntax.md)
- [Example Script](../test.oc)
