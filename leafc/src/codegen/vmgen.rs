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
	/// Set the instruction pointer
	Jump(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator {
	instructions: Vec<Instruction>,
}

impl CodeGenerator {
	pub fn new() -> Self {
		CodeGenerator {
			instructions: Vec::new(),
		}
	}

	pub fn gen_instructions(mut self, ast: SyntaxTree) -> Vec<Instruction> {
		self.gen_from_ast(&ast);

		self.instructions
	}

	/// Generate instructions for a block
	pub fn gen_from_ast(&mut self, ast: &SyntaxTree) {
		// Start a new stack frame
		self.instructions.push(Instruction::Frame);
		// Generate instructions for each statement in the block
		for statement in &ast.block {
			self.gen_from_stmnt(statement);
		}
		// Generate instructions for the block output (if there is one)
		if let Some(ref output) = ast.output {
			// Load the value of the exression onto the stack
			self.gen_from_expr(output);
			// Return the value into the previous stack frame
			self.instructions.push(Instruction::Return);
		} else {
			// Return a nil value
			// This is necessary because if the block was a statement then it will always drop the result
			self.instructions.push(Instruction::Push(Value { val: 0 }));
			self.instructions.push(Instruction::Return);
		}
		// Exit the stack frame
		self.instructions.push(Instruction::Exit);
	}

	/// Generate the instructions for a statement
	pub fn gen_from_stmnt(&mut self, stmnt: &Statement) {
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
					self.gen_from_expr(expr);
					// Bind the variable to the current stack pointer
					self.instructions.push(Instruction::Bind(ident.to_owned()));
				} else {
					// Push a nil value to the stack
					self.instructions.push(Instruction::Push(Value { val: 0 }));
					// Bind the variable to the nil value
					self.instructions.push(Instruction::Bind(ident.to_owned()));
					unimplemented!();
					// Unimplemented because binding a variable to default is not supported right now
				}
			},
			Statement::Debug(ref expr) => {
				// Generate the expression instructions
				self.gen_from_expr(expr);
				// Debug the value at the top of the stack (pops it automatically)
				self.instructions.push(Instruction::Debug)
			},
			Statement::Expression(ref expr) => {
				// Generate instructions from expression
				self.gen_from_expr(expr);
				// Pop the unused result from the stack
				self.instructions.push(Instruction::Pop);
			},
		}
	}

	/// Generate instructions from an expression
	pub fn gen_from_expr(&mut self, expr: &Expression) {
		match expr {
			// Push literal values onto the stack
			Expression::NumberLiteral(num) => {
				self.instructions.push(Instruction::Push(Value { val: *num }))
			},
			// Generate instructions for nested blocks
			Expression::Block(ref ast) => {
				self.gen_from_ast(ast);
			},
			// Dereference variables
			Expression::Identifier(ref ident) => {
				// Push the value of the variable onto the top of the stack
				self.instructions.push(Instruction::Load(ident.to_owned()))
			},
			// Evalute binary expressions
			Expression::Binary { left, right, op } => {
				// Push the value of the left hand side to the stack
				self.gen_from_expr(left);
				// Push the value of the right hand side to the stack
				self.gen_from_expr(right);
				// Perform the operation
				match op {
					BinaryOp::Add => self.instructions.push(Instruction::Add),
					BinaryOp::Assign => {
						panic!("Assign binary operator found. This should have been converted to a standalone\
						expression during the parsing phase, not a binary operation")
					},
					_ => unimplemented!()
				};
			},
			Expression::Assign(ref assignment) => {
				// Load the value that used to be in the variable being assigned to
				self.instructions.push(Instruction::Load(assignment.ident.to_owned()));
				// Push the result of the expr onto the stack
				self.gen_from_expr(&assignment.expr);
				// Move the result into the variable
				self.instructions.push(Instruction::Set(assignment.ident.to_owned()));
				// And the old value will be left on the stack as the result of the expression
			},
			Expression::Loop(ref block) => {
				// Record the instruction pointer for the start of the loop
				let loop_start = self.instructions.len() - 1;
				// Push the instructions for the block
				self.gen_from_ast(block);
				// Push the jump instruction to the previously recorded index
				self.instructions.push(Instruction::Jump(loop_start));
			},
			_ => unimplemented!(),
		}
	}
}

pub struct ByteGen {}

impl ByteGen {
	pub fn gen_bytes(_instructions: &[Instruction]) {
		unimplemented!()
	}
}
