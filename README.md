# Mini Language Compiler

A compiler implementation for a custom programming language that translates source code into 32-bit x86 assembly. This project demonstrates compiler design principles, type systems, and systems programming concepts.

## Project Overview

This compiler implements a statically-typed programming language with support for:

- Primitive types (integer, boolean, char)
- Pointer types (intptr, charptr)
- String arrays
- Control flow (if-else, while loops)
- Functions and procedures
- Array operations

The compiler pipeline includes lexical analysis, parsing, semantic analysis, and code generation phases, producing optimized 32-bit x86 assembly output.

## File Descriptions

- **ast.hpp/cpp**: Abstract Syntax Tree implementation using inheritance and the visitor pattern for tree traversal
- **attribute.hpp**: Type definitions and attribute handling for the compiler
- **codegen.cpp**: 32-bit x86 assembly code generation with register allocation and stack management
- **lexer.l**: Lexical analyzer specification using Flex, handling tokens and literals
- **parser.ypp**: Parser specification using Bison, implementing the language grammar
- **primitive.hpp/cpp**: Implementation of primitive types and their operations
- **symtab.hpp/cpp**: Symbol table implementation for scope management and variable tracking
- **typecheck.cpp**: Type checking system ensuring type safety and proper scoping

## Build and Run

```bash
make        # Build the compiler
./compiler  # Run the compiler
```

## 🎓 Learning Outcomes

This project demonstrates proficiency in:

- Compiler design and implementation
- Systems programming
- C++ programming
- Memory management
- Type systems
- Assembly language
- Software architecture

## 🔍 Future Improvements

Potential areas for enhancement:

- Optimization passes
- Additional language features
- Better error recovery
- Enhanced debugging support
- Performance optimizations
