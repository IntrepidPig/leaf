## Leaf Grammar

This is an attempt at writing a grammar for leaf, and since leaf doesn't actually exist yet, it's currently more like a very incomplete grammar for Rust.

```
- Statement -> `let` Identifier `=` Expression`;` | Expression`;`
- TypeAnn -> `: ` Type
- Type -> 'TypeName' Option<TypeParameters>
- TypeParameters -> `<` List<Type> `>`
- Identifier -> Tuple<Identifier> | 'IdentifierName' Option<TypeAnn>
- Tuple<T> -> `(`List<T>`)`
- List<T> -> T | T`,` List<T>
- Sequence<T> -> T | T Sequence<T>
- Expression -> Identifier | Literal | Block | Expression Binop Expression | Expression Postop | Preop Expression | Expression MethodCall
- Binop -> + | - | * | /
- Postop -> ? | !
- Preop -> - | +
- MethodCall -> `.`'MethodName'`(`Option<List<Expression>>`)`
- Option<T> ε | T
- Block -> Expression | `{` Sequence<Statement> Option<Expression> `}`
```