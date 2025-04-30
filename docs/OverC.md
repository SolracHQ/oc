# OverC Language

OverC is a programming language that acts as a transpiler to C, providing a modern syntax while leveraging C's performance and portability.

## Overview

OverC allows developers to write code in a more expressive syntax while still having access to C's ecosystem and performance. The language transpiles directly to C code, meaning it can be used anywhere C is supported.

## Basic Syntax

### Hello World Example

```overc
#[entrypoint]
pub fun main(): Int32 {
  let hello: String = "Hello World"
  printf(c"%s\n", hello)
  return 0
}
```

### External Function Bindings

Use the `#[include]` annotation to bind to external C functions:

```overc
#[include(from="<stdio.h>", name="printf")]
fun printf(format: CString, _: CVarArgs)
```

## Data Types

OverC supports the following primitive types:

| Type | Description |
|------|-------------|
| `Int` | Default integer type |
| `Int8`, `Int16`, `Int32`, `Int64` | Sized integer types |
| `UInt`, `UInt8`, `UInt16`, `UInt32`, `UInt64` | Unsigned integer types |
| `Float` | Default floating-point type |
| `Float32`, `Float64` | Sized floating-point types |
| `Boolean` | Boolean type (true/false) |
| `Char` | Character type |
| `String` | String type (OverC native) |
| `CString` | C-compatible string type |
| `[Type, N]` | Fixed-size array type |
| `[Type]` | Slice type (array with length) |
| `*Type` | Pointer type |
| `CVarArgs` | Variable arguments for C functions |
| `void` | No return type |
| `VarArgs` | Variable arguments for OverC functions |

## Variable Declarations

### Immutable Variables (let)

```overc
let name: String = "John"
let age: Int = 30
```

### Mutable Variables (var)

```overc
var counter: Int = 0
counter = counter + 1
```

## Functions

### Function Declaration

```overc
fun add(a: Int, b: Int): Int {
  return a + b
}
```

### Public Functions

```overc
pub fun greet(name: String) {
  printf(c"Hello, %.*s\n", name.len, name.ptr)
}
```

## Annotations

OverC supports annotations for statements, variables, and functions. For example:

```overc
#[entrypoint]
pub fun main(): Int32 {
  // ...existing code...
}
```

## Identifier Rules

Identifiers cannot start with an underscore (`_`) to avoid conflicts with mangled symbols.

## Complete Example

```overc
#[include(from="<stdio.h>", name="printf")]
fun printf(format: CString, _: CVarArgs)

#[entrypoint]
pub fun main(): Int32 {
  var number: Int32 = 8
  printf(c"%d\n", number)
  return 0
}
```