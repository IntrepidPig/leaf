# LIL: Leaf Intermediate Language

LIL is an intermediate representation of a Leaf program between the AST and bytecode.

## Structure

LIL is composed of functions and types.

### Functions

A LIL function is a set of blocks, as well as a root block that is executed first when the function is called. A function can take a fixed amount of arguments with a specified type, and can return a single value with a specified type. A function is guaranteed to not diverge unless it is explicitly specified to be able to do so.

### Blocks

A block is made of a list of statements, and a terminating statement. Each statement will be executed in order. The only way a statement can leave the current block is by executing a function call, which is guaranteed to return back to the current position (it is not diverging). A statement that is not the terminating statement can only execute non-diverging functions. The terminating statement is able to jump to different places based on the current state of the program. All of the statements in a block are guaranteed to execute if the block is called.

### Types

A type is either a primitive, or a list of types. Every type has a known size. The size of a primitive is defined by the compiler. The size of a non-primitive type can be computed by summing the size of the types it contains.

### Memory Interface

Most programming languages have a concept of lvalues and rvalues to represent variables, values, and what you can to them. An lvalue is literally a value that can go on the left side of an assignment expression, i. e. be assigned to. For example, in `x = a`, `x`, is an lvalue. Similarly, `a` is an rvalue. The reason this distinction is made is because it's common for expressions to evalute to something that is assignable, but not always. For example, the expression for a field of a data structure, like `foo.bar`, is an expression that can be assigned a different value rather than simply evaluate to this value. The reason there is a distinction between lvalues and rvalues is that not all expressions are assignable. For example, `3 + 4` does not refer to a memory location that can be modified, it's simply a value.

Since I've always found the distinction between lvalues and rvalues somewhat confusing (all lvalues can be rvalues, but not vice versa, the index operator `[]` is almost always a special case, etc), I've decided to take a different approach in Leaf. I'm not exactly sure about how it'll be different, but I know I want it to be somehow different and better. I'm thinking of making a distinction between memory locations, and literal values, which technically would still be a memory location, but it would be compiled into the binary and never be changed. As I write this I realize the more important thing is whether a value is located on the stack or on the heap. As I write that I realize that stack data can be persistent, mutable, and rvalueish too. But wait! Leaf took inspiration from Java and has separate places for local variables on the stack and intermediate calculations for things on the stack. So we can distinguish lvalues and rvalues in Leaf by knowing whether they are stored on the value stack, the local variable stack, or maybe when support is added on the heap or something.