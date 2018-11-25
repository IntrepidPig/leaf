## Compilation stages

There should be a few stages to compilation.

### Parsing

This is the construction of an AST from a textual input. The AST should be only a structured version of the textual input. No lexical information should be lost in the AST other than formatting.

### Analysis

This is the construction of HIR from the AST. The gist of this stage of compilation is resolving mysteries in the AST. The construction of the HIR is where type inference occurs. Any reference to an element of the program in HIR is done by an absolute module path. The type of all expressions in HIR is known and stored in the HIR representation. This representation will probably be helpful for 

### Understanding

This is the construction of MIR from HIR. MIR is a lot more basic than HIR. In MIR, all modules are flattened out and everything is referred to by its absolute path 