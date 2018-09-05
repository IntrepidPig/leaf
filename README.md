# Leaf

Leaf is a programming language I'm writing in my free time that focuses on correctness and usability. Programs are compiled to Leaf bytecode, which can then be executed by the leaf bytecode interpreter. The syntax of the language is based on expressions.

## Syntax

A basic Leaf program looks like this:

```
use ::core::Int;

fn main() {
	factorial(5) == 120
}

fn factorial(n: Int): Int {
	if { n == 1 } then { 1 } else { n * factorial(n - 1) }
}
```

More examples can be found in `leafc/tests/programs/`.

## Execution

This repository includes two command line utilites. `leafc` is used to compile Leaf programs, and `leaf` can run the output produced by `leafc`. These are just thin wrappers over libraries that can be used from other programs as well.

To quickly compile and execute a Leaf program, use pipes:

```
cat factorial.leaf | leafc - - | leaf -
```

The dashes represent either `stdin` or `stdout` and can be replaced with file names, eliminating the need for pipes. For example:

```
leafc factorial.leaf factorial.lfb
leaf factorial.lfb
```

## More Info

Documents describing the language in more depth can be found in the `lang/` folder.