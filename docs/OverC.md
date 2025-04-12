# OverC Language

OverC is a programming language that acts as a transpiler to C, providing a modern syntax while leveraging C's performance and portability.

## Overview

OverC allows developers to write code in a more expressive syntax while still having access to C's ecosystem and performance. The language transpile directly to C code, meaning it can be used anywhere C is supported.

## Basic Syntax

### Hello World Example

```overc
pub fun main(): Int {
  let hello: String = "Hello World"
  printf("%s\n", CString(hello))
  return 0
}
```

### External Function Bindings

Use the `#[extern]` attribute to bind to external C functions:

```overc
#[extern(header="<stdio.h>", name="printf")]
fun printf(format: *Char, args: CVarArgs)
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

## Complete Example

```overc
#[extern(header="<stdio.h>", name="printf")]
fun printf(format: *Char, args: CVarArgs)

#[entrypoint]
pub fun main(): Int {
  var number: Int = 8
  printf("%d\n", number)
  return 0
}
```