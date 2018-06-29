use ast::parser::{BinaryOp, Binding, Expression, Statement, SyntaxTree};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
	pub val: u64,
}

/// An instruction/opcode for the vm
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	/// New stack frame
	Frame,
	/// Exit the stack frame, dropping all the values it had
	Exit,
	/// Push a value to the stack
	Push(Value),
	/// Pop the top of the stack into the top of the previous stack frame
	Return,
	/// Bind a variable to the current stack pointer
	Bind(String),
	/// Load the value of a variable to the top of the stack
	Load(String),
	/// Pop the top of the stack value into the address pointed to by a variable
	Set(String),
	/// Pop the top of the stack into oblivion
	Pop,
	/// Pop the top two values on the stack and push their sum back
	Add,
	/// Print the value at the top of the stack and pop it
	Debug,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator {}

impl CodeGenerator {
	pub fn new() -> Self {
		CodeGenerator {}
	}

	pub fn gen_instructions(&mut self, ast: SyntaxTree) -> Vec<Instruction> {
		let instructions = self.gen_from_ast(&ast);

		instructions
	}

	/// Generate instructions for a block
	pub fn gen_from_ast(&mut self, ast: &SyntaxTree) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		// Start a new stack frame
		instructions.push(Instruction::Frame);
		// Generate instructions for each statement in the block
		for statement in &ast.block {
			instructions.append(&mut self.gen_from_stmnt(statement));
		}
		// Generate instructions for the block output (if there is one)
		if let Some(ref output) = ast.output {
			// Load the value of the exression onto the stack
			instructions.append(&mut self.gen_from_expr(output));
			// Return the value into the previous stack frame
			instructions.push(Instruction::Return);
		} else {
			// Return a nil value
			// This is necessary because if the block was a statement then it will always drop the result
			instructions.push(Instruction::Push(Value { val: 0 }));
			instructions.push(Instruction::Return);
		}
		// Exit the stack frame
		instructions.push(Instruction::Exit);

		instructions
	}

	/// Generate the instructions for a statement
	pub fn gen_from_stmnt(&mut self, stmnt: &Statement) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match stmnt {
			Statement::Binding(Binding {
				ref ident,
				val: expr,
				..
			}) => {
				// If there's an expression
				if let Some(expr) = expr {
					// Generate the instructions from the expression
					// This will push the result onto the stack
					instructions.append(&mut self.gen_from_expr(expr));
					// Bind the variable to the current stack pointer
					instructions.push(Instruction::Bind(ident.to_owned()));
				} else {
					// Push a nil value to the stack
					instructions.push(Instruction::Push(Value { val: 0 }));
					// Bind the variable to the nil value
					instructions.push(Instruction::Bind(ident.to_owned()));
					unimplemented!();
					// Unimplemented because binding a variable to default is not supported right now
				}
			},
			Statement::Debug(ref expr) => {
				// Generate the expression instructions
				instructions.append(&mut self.gen_from_expr(expr));
				// Debug the value at the top of the stack (pops it automatically)
				instructions.push(Instruction::Debug)
			},
			Statement::Expression(ref expr) => {
				// Generate instructions from expression
				instructions.append(&mut self.gen_from_expr(expr));
				// Pop the unused result from the stack
				instructions.push(Instruction::Pop);
			},
		}

		instructions
	}

	/// Generate instructions from an expression
	pub fn gen_from_expr(&mut self, expr: &Expression) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match expr {
			// Push literal values onto the stack
			Expression::NumberLiteral(num) => {
				instructions.push(Instruction::Push(Value { val: *num }))
			},
			// Generate instructions for nested blocks
			Expression::Block(ref ast) => {
				instructions.append(&mut self.gen_from_ast(ast));
			},
			// Dereference variables
			Expression::Identifier(ref ident) => {
				// Push the value of the variable onto the top of the stack
				instructions.push(Instruction::Load(ident.to_owned()))
			},
			// Evalute binary expressions
			Expression::Binary { left, right, op } => {
				// Push the value of the left hand side to the stack
				instructions.append(&mut self.gen_from_expr(left));
				// Push the value of the right hand side to the stack
				instructions.append(&mut self.gen_from_expr(right));
				// Perform the operation
				match op {
					BinaryOp::Add => instructions.push(Instruction::Add),
					BinaryOp::Assign => {
						panic!("Assign binary operator found. This should have been converted to a standalone\
						expression during the parsing phase, not a binary operation")
					},
					_ => unimplemented!()
				};
			},
			Expression::Assign(ref assignment) => {
				// Load the value that used to be in the variable being assigned to
				instructions.push(Instruction::Load(assignment.ident.to_owned()));
				// Push the result of the expr onto the stack
				instructions.append(&mut self.gen_from_expr(&assignment.expr));
				// Move the result into the variable
				instructions.push(Instruction::Set(assignment.ident.to_owned()));
				// And the old value will be left on the stack as the result of the expression
			},
			_ => unimplemented!(),
		}

		instructions
	}
}

pub struct ByteGen {}

impl ByteGen {
	pub fn gen_bytes(_instructions: &[Instruction]) {
		unimplemented!()
	}
}
