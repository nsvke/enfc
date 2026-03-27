# ENFC: A Statically Typed Compiler Implementation in Rust

ENFC is an experimental, statically typed, imperative programming language compiler. The project serves as a comprehensive study into modern compiler architecture, focusing on building every stage of the compilation pipeline—from lexical analysis to code generation—without the use of parser generators or high-level DSLs.

## Architectural Overview

### Frontend

- **Lexical Analysis:** A custom, iterator-based tokenizer designed for precision and clarity in error reporting.
- **Parsing:** A hand-written recursive descent parser. It employs **Pratt Parsing** for expression precedence and implements robust **error recovery** mechanisms to provide multiple diagnostic messages in a single pass.
- **Abstract Syntax Tree (AST):** A structured representation of the program's imperative constructs, currently supporting variable/function declarations, control flow (if/while), and nested scopes.

### Diagnostics

- **Error Infrastructure:** Integrated diagnostic reporting using `miette`. The compiler provides rich, span-aware error messages with source code snippets to facilitate debugging of the source language.

### Middle-end (Completed)

- **Semantic Analysis & Type Checking:** A static type checker that transforms the raw AST into a Typed AST.
- **Intermediate Representation Generation:** A custom stack based IR.

## Backend (Development in Progress)

- **C Transpilation:** Currently implementing a compiler backend that consumes the IR. It simulates a virtual stack at compile-time to translate IR instructions into equivalent, highly optimized, native C statements and expressions.

## Roadmap and Future Goals

- **FFI Support**
- **WARN diagnostic**
- **Advanced Types:** Structs, enums, arrays, and pointer semantics.

## Build and Usage

```bash
# Clone the repository
git clone https://github.com/nsvke/enfc

# Run the compiler on a source file
cargo run -- -s <path_to_source_file>

# Execute the test suite
cargo test
```

## 📝 License

Distributed under the MIT License. See `LICENSE` for more information.
