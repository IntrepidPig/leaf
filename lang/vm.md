## List of instructions implemented in the VM

- Push Value - Pushes a value to the top of the stack
- Pop - Pops the top value off the stack, sending it into oblivion
- Bind Variable - Binds an variable to the current stack pointer
- Load Variable - Copies the value a variable points to to the top of the stack
- Drop Variable - Destroys a variable from memory
- Debug - Prints the value at the top of the stack
- Frame - Enter a new stack frame
- Return - Move the value at the top of the stack to the top of stack frame below it
- Exit - Delete the top stack frame and drop all the variables in it
- Set Variable - Move the value at the top of the stack to the address pointed to by a variable
- Add - Pop the top two values off the stack and push the result of adding them together onto the stack