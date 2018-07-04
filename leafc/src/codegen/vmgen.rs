use ast::parser::{Block, Expression, If};
use ast::parser::operators::*;

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
	/// Jump to the location if the top of the stack is false
	Check(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator {
	instructions: Vec<Instruction>,
	loop_breaks: Vec<Vec<usize>>,
	block_is_loop: bool,
	if_jumps: Vec<usize>,
}

impl CodeGenerator {
	pub fn new() -> Self {
		CodeGenerator {
			instructions: Vec::new(),
			loop_breaks: Vec::new(),
			block_is_loop: false,
			if_jumps: Vec::new(),
		}
	}

	pub fn gen_instructions(mut self, ast: Block) -> Vec<Instruction> {
		self.gen_from_block(&ast);

		self.instructions
	}

	/// Generate instructions for a block
	pub fn gen_from_block(&mut self, ast: &Block) {
		// Start a new stack frame
		self.instructions.push(Instruction::Frame);
		// Generate instructions for each statement in the block
		for statement in &ast.block {
			self.gen_from_expr(statement);
			// Pop the unused result of the expression
			self.instructions.push(Instruction::Pop);
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

	pub fn gen_from_loop(&mut self, body: &Expression) {
		// Record the start of the loop
		let loop_start = self.instructions.len();
		// Start a list of breaks for this loop
		self.loop_breaks.push(Vec::new());
		// Gen the instructions for each statement
		self.gen_from_expr(body);
		// Pop the unused result
		self.instructions.push(Instruction::Pop);
		// Jump back to the beginning
		self.instructions.push(Instruction::Jump(loop_start));
	}

	pub fn gen_from_if(&mut self, if_stmnt: &If) {
		// Condition at top of stack
		self.gen_from_expr(&if_stmnt.condition);
		// Location of check instruction
		let if_check = self.instructions.len();
		// Temporary placeholder check instruction
		self.instructions.push(Instruction::Check(0));
		// The if body right after the check instruction, happens if check was true
		self.gen_from_expr(&if_stmnt.body);
		// Jump to the end of the else clause after the if clause executes
		// The location of the jump to after the else clause
		let jump_to_after_else = self.instructions.len();
		// Placeholder jump to after else block
		self.instructions.push(Instruction::Jump(0));

		// Fix the check instruction jump locations
		let current_ip = self.instructions.len();
		match self.instructions[if_check] {
			Instruction::Check(ref mut ptr) => *ptr = current_ip,
			_ => panic!("Failed to change check instruction pointer"),
		}

		// Generate stuff for the else block if it exists, otherwise just generate nil value
		if let Some(else_block) = &if_stmnt.else_block {
			self.gen_from_expr(else_block);
		} else {
			self.instructions.push(Instruction::Push(Value { val: 0 }));
		}

		// Fix the jump to after else locations
		let current_ip = self.instructions.len();
		match self.instructions[jump_to_after_else] {
			Instruction::Jump(ref mut ptr) => *ptr = current_ip,
			_ => panic!("Failed to change check instruction pointer"),
		}
	}

	/// Generate instructions from an expression
	pub fn gen_from_expr(&mut self, expr: &Expression) {
		match expr {
			// Push literal values onto the stack
			Expression::NumberLiteral(num) => self.instructions
				.push(Instruction::Push(Value { val: *num })),
			// Generate instructions for nested blocks
			Expression::Block(ref ast) => {
				self.gen_from_block(ast);
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
				self.instructions
					.push(Instruction::Load(assignment.ident.to_owned()));
				// Push the result of the expr onto the stack
				self.gen_from_expr(&assignment.expr);
				// Move the result into the variable
				self.instructions
					.push(Instruction::Set(assignment.ident.to_owned()));
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
						_ => panic!("Failed to change all break statements"),
					}
				}
			},
			Expression::If(ref if_stmnt) => {
				// Gen the instructions for the if statement
				self.gen_from_if(if_stmnt);
			},
			Expression::Binding(ref binding) => {
				// If there's an expression
				if let Some(ref expr) = binding.val {
					// Generate the instructions from the expression
					// This will push the result onto the stack
					self.gen_from_expr(expr);
					// Bind the variable to the current stack pointer
					self.instructions
						.push(Instruction::Bind(binding.ident.to_owned()));
				} else {
					// Push a nil value to the stack
					self.instructions.push(Instruction::Push(Value { val: 0 }));
					// Bind the variable to the nil value
					self.instructions
						.push(Instruction::Bind(binding.ident.to_owned()));
					unimplemented!();
					// Unimplemented because binding a variable to default is not supported right now
				}
				// Push a nil value since let is an expression and it's result will be popped
				self.instructions.push(Instruction::Push(Value { val: 0 }))
			},
			Expression::Debug(ref expr) => {
				// Generate the expression instructions
				self.gen_from_expr(expr);
				// Debug the value at the top of the stack (pops it automatically)
				self.instructions.push(Instruction::Debug)
			},
			Expression::Break(ref expr) => {
				// If there's an associated expression then gen instructions for it, otherwise just push a nil
				if let Some(expr) = expr {
					self.gen_from_expr(expr);
				} else {
					self.instructions.push(Instruction::Push(Value { val: 0 }))
				}
				// Return the value being broken and exit the loop stack frame
				self.instructions.push(Instruction::Return);
				self.instructions.push(Instruction::Exit);

				// Push a jump instrction with a 0 because we don't know where the loop ends yet
				self.instructions.push(Instruction::Jump(0));
				// Add the index of the jump instruction to the latest loop_break frame
				self.loop_breaks
					.last_mut()
					.unwrap()
					.push(self.instructions.len() - 1);
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
