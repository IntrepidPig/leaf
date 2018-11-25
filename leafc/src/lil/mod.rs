pub mod gen;
pub mod instruction;

use ::lil::instruction::LILInstruction;

/// LIL (Leaf Intermediate Language) is a block based intermediate representation for a Leaf
/// program during compilation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LIL {
	pub typedefs: Vec<TypeDef>,
	pub functiondefs: Vec<FunctionDef>,
}

/// Represents a definition of something (T) in a package.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageItem<T> {
	/// The name of the package that contains the definition, None means the current package
	package: Option<String>,
	/// The name of the item being defined
	name: String,
	_type: ::std::marker::PhantomData<T>,
}

impl<T> PackageItem<T> {
	pub fn new(package: Option<String>, name: String) -> PackageItem<T> {
		PackageItem {
			package,
			name,
			_type: ::std::marker::PhantomData,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
	ident: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDef {
	pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDef {
	pub locals: usize,
	pub root_block: Block,
	pub blocks: Vec<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	pub statements: Vec<Statement>,
	pub terminator: Option<Terminator>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
	Instruction(LILInstruction),
	Assignment(Assignment),
	Call(Call),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
	Statement(Statement),
	/// Jump to block if condition is false
	Check(Value),
	/// Jump to a block
	Jump(),
	Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
	pub function: PackageItem<FunctionDef>,
}

/// Represents an assignment of a value of any source to a storage location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
	pub target: Storage,
}

/// Represents any readable value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
	Storage(Storage),
	Literal(Literal),
	Intermediate,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
	Int(i32),
	Bool(bool),
}

/// Represents a memory location that is the persistent storage of some value such
/// as a local variable or a pointer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Storage {
	Local(Local),
}

/// Represents a local variable in a function by it's index in the function's
/// list of local variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local {
	pub index: usize,
}