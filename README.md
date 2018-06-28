# Leaf

Leaf is a toy programming language that I'm creating as a learning experience. Right now it can't do much, but hopefully it eventually gets to a point where it might be useful to someone. It is compiled to bytecode by `leafc` which is then interpreted by the VM, `leaf`, both of which are implemented in Rust. Currently, a bytecode representation isn't reached, but instead the `leaf` uses `leafc` as a library to compile it to the assembly-like representation and no further. 

At the moment, the entirety of the language can be encapsulated into a small example:

```
let a = 5;
let b = {
	a + { 2 + 4 }
};
{
	let q = 5;
	debug q;
};
debug a;
debug q;
```

This program prints

```
Value: 5
Value: 11
```

and then crashes. The reason for that is, like many other languages, variables are confined to the scope they are created in. When the interpreter reaches the end of the scope, it forgets about (drops) all the variables that were created in that scope. If your familiar with rust, then you can see that this language was heavily inspired by it.

You can also see that it is expression oriented, and that blocks can return a value which can be used in assignments. The example above shows a block that returns the value of `c`, which is then bound to `b`.

## Type System?
The only type available for now is the unsigned 64 bit integer. There's not even a concept of types. I plan on making a type system based on the `Root` type, which is empty. Every type has `Root` type within it. Types will also be able to have other types within them. This will be my implementation of composition. The compiler will help a lot to make polymorphic programming strategies available, and types will all be dynamically allocated on the physical heap to allow for dynamic dispatch without lots of boilerplate, at the cost of performance.
