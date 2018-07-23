## Types implementation details

After getting my hands dirty in implementing types, I discovered a few things.

- Dynamic typing is easier to implement than static typing
- Simple type inference isn't so hard, and actually kind of fell into place
- Types require you to finally solve the problem of primitives
- They also kind of require you to implement some sort of namespacing
- To enable dynamic typing, they need some sort of id at runtime, which is preferably not a string

So, now I have some plans set. I'll generate a table of types which is accessable at runtime, which will be a Vec. Each type will have a table of values and a table of functions, also stored as Vecs. When compiling, types will be evaluated to indices in the type table, and field accesses will be converted to indices in the field table, and function calls will be converted to indices in the function table. 

### Primitives

Once a module system is implemented, I think I will create a core module with all of its members automatically included in every other module. It will have primitive types like Int and Bool declared in it, but they won't have any fields. They'll also have the methods they have implemented for them. Operators on primitives will invoke compiler specialization, but the methods can be used as well.