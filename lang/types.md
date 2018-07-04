## Types

It's time. No more putting it off. There has to be a type system. I just need to figure out how.

I kind of wanted it to be without primitive types, but I think there's no way. I plan on starting with just the I32 primitive type, and then I'll to the Bool primitive type. Instead of arrays as a primitive type, there will be a List primitive type that will be a growable list of any type or multiple types. I tend to sacrifice lots of performance for ease of implementation. Everything other than the primitive types will be Reference types, pointers to the heap where collections of primitive types and reference types are stored (as classes/structs).

This seems kind of dumb, but I think I'm going to have to implement types internally as an enum, like (Null | Primitive(I32 | Bool | List) | Reference(ptr)), which is dumb because they'll all take up the same amount of space. It might also be more efficient to just have a usize and rely on leafc to produce bytecode that interprets each usize correctly. That would cut off the need for a discriminant.

### Null

I think I'll eventually make types nullable like in Kotlin, but for now everything will just be nullable by default.

### Polymorphism

Oh boy. I really have no idea what I'm doing here. I wanted to do something like composition without traits/interfaces and types are just composed of other types and can be automatically used as those types if specified, but then I realized that's just multiple inheritance. It'd be a little different though because each data type would have fields, and it can specify one field of a certain type as something it has. It would look something like

```
// Data type. Can be part of other data types
type Wheel {
	...
}

// Abstract type. Has associated methods that can be overridden, but no fields
abstract type (aka trait) Vehicle {
	...
}

// Data type.
type DriverSeat {

}

// Data type, with other data types
type Car {
	has DriverSeat
	is Vehicle
	wheel1: Wheel
	wheel2: Wheel
	wheel3: Wheel
	wheel4: Wheel
}

// Data type, with other data types
type Bike {
	has DriverSeat
	is Vehicle
	wheel1: Wheel,
	wheel2: Wheel,
}
```
