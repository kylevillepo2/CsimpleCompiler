# Mini Language Compiler

A robust compiler implementation for a custom programming language, demonstrating advanced systems programming concepts and compiler design principles.

## 🚀 Features

- **Lexical Analysis**: Custom lexer implementation using Flex (lexer.l) supporting:

  - Multiple number formats (hex, octal, binary, decimal)
  - String and character literals
  - Comprehensive operator support
  - Comment handling

- **Syntax Analysis**: Parser implementation using Bison (parser.ypp) featuring:

  - Strong type system with support for:
    - Primitive types (integer, boolean, char)
    - Pointer types (intptr, charptr)
    - String arrays
  - Control flow structures (if-else, while loops)
  - Function declarations and calls
  - Variable declarations and scoping
  - Array operations

- **Semantic Analysis**: Type checking system that ensures:

  - Type safety across expressions
  - Proper variable scoping
  - Function call validation
  - Pointer operation safety
  - Array bounds checking

- **Code Generation**: MIPS assembly code generation with:
  - Register allocation
  - Stack frame management
  - Procedure calls
  - Memory management
  - String handling

## 🛠️ Technical Implementation

The project is implemented in C++ and demonstrates several key software engineering concepts:

- **Object-Oriented Design**: Extensive use of inheritance and polymorphism for AST nodes
- **Visitor Pattern**: Implementation of the visitor pattern for tree traversal
- **Memory Management**: Custom memory management for AST nodes and symbol tables
- **Error Handling**: Comprehensive error detection and reporting system
- **Symbol Table Management**: Efficient symbol table implementation with scope handling

## 📋 Project Structure

```
.
├── ast.hpp/cpp         # Abstract Syntax Tree implementation
├── attribute.hpp       # Type and attribute definitions
├── codegen.cpp        # MIPS code generation
├── lexer.l            # Lexical analyzer specification
├── parser.ypp         # Parser specification
├── primitive.hpp/cpp  # Primitive type handling
├── symtab.hpp/cpp     # Symbol table implementation
└── typecheck.cpp      # Type checking implementation
```

## 🎯 Key Technical Achievements

1. **Type System**: Implementation of a sophisticated type system supporting:

   - Type inference
   - Type compatibility checking
   - Pointer type safety
   - Array type validation

2. **Memory Management**: Efficient handling of:

   - Stack frames
   - Dynamic memory allocation
   - Pointer operations
   - String literals

3. **Error Detection**: Comprehensive error checking for:
   - Type mismatches
   - Undefined variables
   - Invalid operations
   - Scope violations

## 🏗️ Build and Run

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
