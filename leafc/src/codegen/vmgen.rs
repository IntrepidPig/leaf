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
	loop_breaks: Vec<Vec<usize>>,
	block_is_loop: bool,
}

impl CodeGenerator {
	pub fn new() -> Self {
		CodeGenerator {
			instructions: Vec::new(),
			loop_breaks: Vec::new(),
			block_is_loop: false,
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
			
		} else {
			// Return a nil value
			// This is necessary because if the block was a statement then it will always drop the result
			self.instructions.push(Instruction::Push(Value { val: 0 }));
		}
		// Return the value into the previous stack frame
		self.instructions.push(Instruction::Return);
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
			Statement::Break(ref expr) => {
				// If there's an associated expression then gen instructions for it, otherwise just push a nil
				if let Some(expr) = expr {
					self.gen_from_expr(expr);
				} else {
					self.instructions.push(Instruction::Push(Value { val: 0 }))
				}

				// Push a jump instrction with a 0 because we don't know where the loop ends yet
				self.instructions.push(Instruction::Jump(0));
				// Add the index of the jump instruction to the latest loop_break frame
				self.loop_breaks.last_mut().unwrap().push(self.instructions.len() - 1);
			}
		}
	}

	pub fn gen_from_loop(&mut self, block: &SyntaxTree) {
		// Record the start of the loop
		let loop_start = self.instructions.len();
		// Start a new stack frame
		self.instructions.push(Instruction::Frame);
		// Start a list of breaks for this loop
		self.loop_breaks.push(Vec::new());
		// Gen the instructions for each statement
		for statement in &block.block {
			match statement {
				// Handle break statements
				Statement::Break(ref break_expr_opt) => {
					// Evaluate the expression if there is one
					// Will be returned to the previous stack frame
					if  let Some(break_expr) = break_expr_opt {
						self.gen_from_expr(break_expr);
					} else {
						self.instructions.push(Instruction::Push(Value { val: 0 }))
					}
					// Push a jump instruction to 0 that will be changed once we know where the loop ends
					self.instructions.push(Instruction::Jump(0));
					// Record the index of the todo break instructions
					self.loop_breaks.last_mut().unwrap().push(self.instructions.len() - 1);
				},
				statement => {
					self.gen_from_stmnt(statement);
				}
			}
		}
		// Drop the current stack frame but don't return anything because we're going back to run it again
		self.instructions.push(Instruction::Exit);
		// Jump back to the beginning
		self.instructions.push(Instruction::Jump(loop_start));
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
				// Push the instructions for the block
				self.gen_from_loop(block);
				// The place after the loop
				let loop_end = self.instructions.len();
				// Change each break statement to point to the proper place now that we know where the loop ends
				for break_index in self.loop_breaks.pop().unwrap() {
					match self.instructions[break_index] {
						Instruction::Jump(ref mut target) => *target = loop_end,
						_ => panic!("Failed to change all break statements")
					}
				}
				// Return the value being broken and exit the stack frame
				self.instructions.push(Instruction::Return);
				self.instructions.push(Instruction::Exit);
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
