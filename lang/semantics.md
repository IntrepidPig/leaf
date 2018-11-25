## Ideas

- reads vs writes: Memory can be modified by either reading or writing to a pointer. Reading would load the data a pointer points to to the hot stack in the VM. Writing would load data from the hot stack in the VM to memory.
- for inheritance, instead of objects being on the heap with a vtable, they could be on the stack and be the size of the largest subclass, so like java inheritance implemented as rust enums
- language support for newtypes