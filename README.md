# Csimple Compiler

A compiler implementation for a Csimple (Language manual: https://sites.cs.ucsb.edu/~chris/teaching/cs160/projects/language.html) that translates source code into 32-bit x86 assembly. This project demonstrates compiler design principles, type systems, and systems programming concepts.

## Project Overview

This compiler implements Csimple with support for:

- Primitive types (integer, boolean, char)
- Pointer types (intptr, charptr)
- String arrays
- Control flow (if-else, while loops)
- Functions and procedures
- Array operations

The compiler pipeline includes lexical analysis, parsing, semantic analysis, and code generation phases, producing optimized 32-bit x86 assembly output.

## File Descriptions

- **ast.hpp/cpp**: Abstract Syntax Tree implementation
- **attribute.hpp**: Type definitions and attribute handling 
- **codegen.cpp**: stack based 32-bit x86 assembly code generation 
- **lexer.l**: Lexical analyzer specification using Flex
- **parser.ypp**: Parser specification using Bison
- **primitive.hpp/cpp**: Implementation of primitive types 
- **symtab.hpp/cpp**: Symbol table implementation 
- **typecheck.cpp**: Type checking system


