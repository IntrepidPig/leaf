/// An instruction/opcode for the vm
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	/// Call a function that starts at usize and create a new stack frame. Pop a
	//. certain amount of values from the current stack frame into the next (arguments)
	Call(usize, usize),
	/// New block frame
	Block,
	/// Exit the stack frame, dropping all the values it had
	PushInt(u64),
	/// Pop the top of the stack into the top of the previous stack frame
	/// Exit the stack frame dropping all of the values present
	Output,
	/// Pop the top of the operand stack into the locals stack
	Bind,
	/// Load the value of a variable to the top of the stack
	Load(usize),
	/// Pop the top of the stack value into the address pointed to by a variable
	Set(usize),
	/// Pop the top of the stack into oblivion
	Pop,
	/// Pop the top two values on the stack and push their sum back
	Add,
	/// Pop the top two values on the stack and push their difference back
	Sub,
	/// Pop the top two values on the stack and push their product back
	Mul,
	/// Pop the top two values on the stack and push their quotient back
	Div,
	/// Print the value at the top of the stack and pop it
	Debug,
	/// Set the instruction pointer
	Jump(usize),
	/// Jump to the location if the top of the stack is false
	Check(usize),
	/// Pop two values and push a boolean representing their equality
	Equal,
	/// Get the indexed field of a value
	Retrieve(usize),
	/// Return the value at the top of the stack to the previous stack frame
	Return,
	/// Pop the amount of values on the stack and push a reference to them all
	Ref(usize),
	/// Stop execution of the program
	Terminate,
	/// Push a Root value onto the operand stack
	PushRoot,
	/// Push a boolean onto the operand stack
	PushBool(bool),
	/// Call an external function at the specified index of the symbol table with specified amount of arguments
	/// The first is the index of the library in the library table. 0 will be the root
	/// The second is the index of the symbol in the library's symbol table
	/// The third is the amount of arguments to pass to the function
	ExternCall(usize, usize, usize),
}
