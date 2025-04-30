# OverC

OverC is a personal hobby project aimed at creating a unique programming language that transpiles to C. Its primary goal is to be self-hosting, meaning OverC will eventually be able to compile itself. This project is not about being better than C but about being *my language*, a language that reflects my preferences and ideas.

## Current Status

OverC is under **active development**. The following features are implemented and working:

- **Lexer**: ✅ Complete for the base syntax.
- **Parser**: ✅ Complete for the base syntax.
- **Semantic Analysis**: ✅ Complete for the base syntax.
- **Code Generation**: ✅ Functional.
- **Structs**: ✅ Implemented and working.
- **Pointers**: ✅ Supported.
- **Arrays**: ✅ Supported.
- **Control Flow**: ✅ If/else, while loops.
- **Function Calls**: ✅ Supported.

The following features are **not yet implemented**:

- **Generics**: 🚧 Not implemented.
- **Tagged Unions**: 🚧 Not implemented.
- **Enums**: 🚧 Not implemented.

### Example Programs

The following example programs compile and run successfully with OverC:

- [`test.oc`](./test.oc)
- [`examples/linked_list.oc`](./examples/linked_list.oc)
- [`examples/arrays.oc`](./examples/arrays.oc)

## Usage

### Building OverC

To build the OverC compiler:

```bash
# Build with release optimizations
nimble build -d:release
```

### Using OverC

Once built, you can use OverC to compile your programs:

```bash
# Compile an OverC source file
./oc compile test.oc

# Compile and run an OverC source file in one step
./oc run test.oc
```

### Command-line Options

OverC supports various options to customize compilation:

```bash
# Use a specific C compiler
./oc compile test.oc --compiler=clang

# Pass custom flags to the C compiler
./oc compile test.oc --cflags="-Wall -Werror"

# Build with optimizations
./oc compile test.oc --release

# Static linking
./oc compile test.oc --static
```

## Philosophy

OverC is not designed to compete with C or other programming languages. Its purpose is to be a personal language that includes features I find interesting, such as:

- **Structs** (implemented)
- **Pointers** (implemented)
- **Arrays** (implemented)
- **Generics** (planned)
- **Tagged Unions** (planned)
- **Pattern Matching** (planned)
- **Enums** (planned)

## Why OverC?

The name "OverC" reflects its relationship with C—it builds on C as a foundation but adds features and ideas that make it unique. It's a language for fun, learning, and self-expression.

## Roadmap

See [TODO.md](./TODO.md) for a detailed roadmap and ongoing tasks.

1. Complete the **type system improvements** to enhance type inference and resolution.
2. Implement **generics**, **tagged unions**, and **enums**.
3. Achieve **self-hosting** by writing OverC in OverC.

## Transpiler Phases

Below is a diagram of the transpiler phases:

![Transpiler Phases](docs/design.png)

## Contributing

OverC is a personal project, but if you're interested in contributing or discussing ideas, feel free to reach out!

## License

This project is licensed under the [MIT License](LICENSE).