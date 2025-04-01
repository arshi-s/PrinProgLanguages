# Compiler for TinyL Language Project
The TinyL Compiler project involves creating a recursive descent LL(1) parser and code generator for the TinyL programming language. The goal is to design a compiler that translates TinyL programs into RISC machine instructions. TinyL is a simple expression language that supports assignments and basic I/O operations.

The compiler is responsible for parsing TinyL source code and generating corresponding RISC machine instructions for each operation, including arithmetic, bitwise operations, and I/O operations. 

This project also includes testing the generated RISC code on a virtual machine, which simulates the execution of the compiled program. The virtual machine supports instructions like loading constants into registers, performing arithmetic and bitwise operations, and handling basic input/output operations.

The compiler was implemented in C, focusing on accurate parsing of the TinyL syntax, efficient code generation, and producing correct output when executed on the virtual machine. Key tasks involved writing the recursive descent parser, implementing the code generation for all TinyL constructs, and testing the generated programs with various test cases.
