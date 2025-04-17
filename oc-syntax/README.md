# oc-syntax

VS Code syntax highlighting for the [OverC](../README.md) programming language.

## Features

- Syntax highlighting for OverC source files (`.oc`).
- Highlights keywords, types, literals, operators, annotations, strings, numbers, and comments.
- Makes OverC code easier to read and write in Visual Studio Code.

## Example

```overc
#[entrypoint]
pub fun main(): Int32 {
  let hello: String = "Hello World"
  printf(c"%s\n", hello)
  return 0
}
```

## Requirements

No dependencies. Just install the extension and open your `.oc` files.

## Extension Settings

This extension does not contribute any settings.

## Known Issues

- Only basic syntax highlighting is provided.
- Advanced features like code completion, linting, or error checking are not included.

## How to Install

1. Clone this repository.

```bash
git clone https://github.com/SolracHQ/oc
```

2. Go to extension directory.

```bash
cd oc/oc-syntax
```

3. Install dependencies.

```bash
npm install
```

4. Install @vscode/vsce

```bash
npm install -g @vscode/vsce
```

5. Package the extension.

```bash
vsce package
```

6. Install the extension in VS Code.

```bash
code --install-extension oc-syntax-*.vsix
```
