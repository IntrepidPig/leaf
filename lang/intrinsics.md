## VM Intrinsics

A user of leafvm as a library should be able to define functions (and data structures?) in Rust that are callable from leaf. The leaf application will need to declare the external function in some module with the `extern` keyword, without a body. All `extern` declarations in a a leaf binary will be compiled into the resulting binary's symbol table. When you start the VM in a Rust program, you define a map of function names to functions that take a Vec of leaf objects as an argument and return a leaf object as a result. Then, before the intructions are parsed, the symbol table is parsed from the binary by the VM. If the symbol table declares any functions that the VM doesn't know about, then execution should fail. The program can then use the instruction ExternCall to call a external functions by their index in the symbol table.

Structure of a binary:
1. Link table: List of libraries to link to by name
2. Extern table: List of functions that are to be defined in the VM
3. Instruction table: List of instructions to be executed from 0 by the VM

Structure of a library:
1. Symbol table: List of symbols this library exports by name and their indices in their relevant tables
2. Link table: List of libraries to link by name
3. Extern table: List of functions to be defined by the VM
4. Instruction table: List of instructions the library defines

Questions:
Q: When compiling a binary that links to a library, how does the compiler know when something is in a different library?
X A: The compiler will have access to the source code of a library it links to
+ A: The library will have a list of symbols that it exports
? A: When compiling libraries, something akin to header files is generated as well to be used when compiling a program that links to that library

Q: When compiling a binary that links to a library, how does the compiler know where external calls go
+ A: It will have access to the shared libraries, which will export a symbol table that it can read

Q: When compiling a library that requires VM implementations, how does the compiler know they will be implemented by the VM?
+ A: They will have to be declared with `extern`