## Types in the VM

I have not yet settled on how to represent types in the VM. I see a few options

- Enum that can be either primitives or a reference
- All types, even primitives, are references
- Make it difficult and have the stack be a Vec<u8> and have to calculate type sizes

I also have to figure out how to store reference types. I think a Vec of fields is the way to go. I might also need to store a name though