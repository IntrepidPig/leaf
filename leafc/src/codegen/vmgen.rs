use std::collections::HashMap;

use ast::parser::{Block, Expression, If, SyntaxTree, Function};
use ast::parser::operators::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
	pub var_info: VarInfo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarInfo {
	Null,
	Primitive(Primitive),
	Reference(Reference),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
	Bool(bool),
	U64(u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference {
	pub fields: Vec<Var>,
}

impl Reference {
	pub fn new(fields: Vec<Var>) -> Self {
		Reference {
			fields
		}
	}
}

impl Var {
	pub fn new_u64(val: u64) -> Self {
		Var {
			var_info: VarInfo::Primitive(Primitive::U64(val)),
		}
	}
	
	pub fn null() -> Self {
		Var {
			var_info: VarInfo::Null,
		}
	}
	
	pub fn new_bool(val: bool) -> Self {
		Var {
			var_info: VarInfo::Primitive(Primitive::Bool(val))
		}
	}
	
	pub fn new_ref(reference: Reference) -> Self {
		Var {
			var_info: VarInfo::Reference(reference)
		}
	}
	
	pub fn is_false(&self) -> bool {
		match self.var_info {
			VarInfo::Primitive(Primitive::U64(val)) => val == 0,
			VarInfo::Primitive(Primitive::Bool(val)) => !val,
			_ => panic!("Tried to check if value was false when it doesn't support it"),
		}
	}
}

/// An instruction/opcode for the vm
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	/// Call a function that starts at usize and create a new stack frame. Pop a
	//. certain amount of values from the current stack frame into the next (arguments)
	Call(usize, usize),
	/// New block frame
	Block,
	/// Exit the stack frame, dropping all the values it had
	Push(Var),
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
	Terminate
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator {
	pub instructions: Vec<Instruction>,
	loop_breaks: Vec<Vec<usize>>,
	block_is_loop: bool,
	if_jumps: Vec<usize>,
	locals: Vec<Vec<String>>,
	function_locations: HashMap<String, usize>,
	function_jumps_todo: Vec<(usize, String)>,
	types: HashMap<String, TypeGen>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeGen {
	fields: Vec<(String, String)>,
}

impl CodeGenerator {	
	pub fn new() -> Self {
		CodeGenerator {
			instructions: Vec::new(),
			loop_breaks: Vec::new(),
			block_is_loop: false,
			if_jumps: Vec::new(),
			locals: vec![Vec::new()],
			function_locations: HashMap::new(),
			function_jumps_todo: Vec::new(),
			types: HashMap::new(),
		}
	}

	pub fn gen_instructions(&mut self, ast: &SyntaxTree) {
		for typedef in &ast.types {
			self.types.insert(typedef.name.clone(), TypeGen { fields: typedef.members.clone() });
		}
		
		self.instructions.push(Instruction::Call(0, 0));
		self.function_jumps_todo.push((0, "main".to_owned()));
		self.instructions.push(Instruction::Terminate);
		for func in &ast.functions {
			self.gen_from_func(&func);
		}
		
		self.change_function_jumps();
	}
	
	pub fn change_function_jumps(&mut self) {
		for (location, name) in &self.function_jumps_todo {
			match self.instructions.get_mut(*location).unwrap() {
				Instruction::Call(ref mut target, _argc) => *target = *self.function_locations.get(name).unwrap(),
				_ => panic!("Expected a jump to {} at index {}", name, location)
			}
		}
	} 
	
	pub fn gen_from_func(&mut self, function: &Function) {
		self.locals.push(Vec::new());
		self.function_locations.insert(function.name.clone(), self.instructions.len());
		// start a new stack frame
		self.instructions.push(Instruction::Block);
		// load all of the arguments
		for arg in &function.args {
			self.locals.last_mut().unwrap().push(arg.0.clone());
		}
		// TODO bind arguments
		self.gen_from_block(&function.body);
		// exit stack frame, return value at the top of the stack
		self.instructions.push(Instruction::Return);
		self.locals.pop();
	}

	/// Generate instructions for a block
	pub fn gen_from_block(&mut self, ast: &Block) {
		// Start a new stack frame
		self.instructions.push(Instruction::Block);
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
			self.instructions.push(Instruction::Push(Var::null()));
		}
		// Return the value into the previous stack frame
		// and exit the current one
		self.instructions.push(Instruction::Output);
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
			self.instructions.push(Instruction::Push(Var::null()));
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
				.push(Instruction::Push(Var::new_u64(*num))),
			// Generate instructions for nested blocks
			Expression::Block(ref ast) => {
				self.gen_from_block(ast);
			},
			// Dereference variables
			Expression::Identifier(ref ident) => {
				// Push the value of the variable onto the top of the stack
				self.instructions.push(Instruction::Load(self.locals.last().unwrap().iter().position(|test_name| test_name == ident).unwrap()))
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
					BinaryOp::Equality => self.instructions.push(Instruction::Equal),
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
					.push(Instruction::Load(self.locals.last().unwrap().iter().position(|test_name| test_name == &assignment.ident).unwrap()));
				// Push the result of the expr onto the stack
				self.gen_from_expr(&assignment.expr);
				// Move the result into the variable
				self.instructions
					.push(Instruction::Set(self.locals.last().unwrap().iter().position(|test_name| test_name == &assignment.ident).unwrap()));
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
					// Bind the variable to the current local var location
					self.instructions
						.push(Instruction::Bind);
					// Push the variable name to the local var stack
					self.locals.last_mut().unwrap().push(binding.ident.clone())
				} else {
					// Push a nil value to the stack
					self.instructions.push(Instruction::Push(Var::null()));
					// Bind the variable to the nil value
					self.instructions
						.push(Instruction::Bind);
					// Push the variable name to the local var stack
					self.locals.last_mut().unwrap().push(binding.ident.clone());
				}
				// Push a nil value since let is an expression and it's result will be popped
				self.instructions.push(Instruction::Push(Var::null()))
			},
			Expression::Debug(ref expr) => {
				// Generate the expression instructions
				self.gen_from_expr(expr);
				// Debug the value at the top of the stack (pops it automatically)
				self.instructions.push(Instruction::Debug);
				// Push a nil value since debug is an expression and it's result will be popped
				self.instructions.push(Instruction::Push(Var::null()))
			},
			Expression::Break(ref expr) => {
				// If there's an associated expression then gen instructions for it, otherwise just push a nil
				if let Some(expr) = expr {
					self.gen_from_expr(expr);
				} else {
					self.instructions.push(Instruction::Push(Var::null()))
				}
				// Return the value being broken and exit the loop stack frame
				self.instructions.push(Instruction::Output);

				// Push a jump instrction with a 0 because we don't know where the loop ends yet
				self.instructions.push(Instruction::Jump(0));
				// Add the index of the jump instruction to the latest loop_break frame
				self.loop_breaks
					.last_mut()
					.unwrap()
					.push(self.instructions.len() - 1);
				// Push a nil value since break is an expression and it's result will be popped
				self.instructions.push(Instruction::Push(Var::null()))
			},
			Expression::BoolLiteral(val) => {
				self.instructions.push(Instruction::Push(Var::new_bool(*val)))
			},
			Expression::FunctionCall { name, args } => {
				for arg in args {
					self.gen_from_expr(arg);
				}
				let function_call_index = self.instructions.len();
				self.function_jumps_todo.push((function_call_index, name.clone()));
				self.instructions.push(Instruction::Call(0, args.len()));
			},
			Expression::FieldAccess(ref expr, ref fieldname) => {
				self.gen_from_expr(expr);
				let exprtype = expr.get_type();
				let typedef = self.types.get(&exprtype).expect("Type not found in list of typedefs");
				let typeindex = typedef.fields.iter().position(|field| &field.0 == fieldname).expect("Type does not have this field");
				self.instructions.push(Instruction::Retrieve(typeindex));
			},
			Expression::Instantiation(ref typename, ref fields) => {
				let typedef = self.types.get(typename).expect("Type with that name not found").clone();
				if fields.len() != typedef.fields.len() {
					panic!("Incorrect amount of fields provided for type")
				}
				for i in 0..fields.len() {
					self.gen_from_expr(&fields[i]);
				}
				self.instructions.push(Instruction::Ref(fields.len()))
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
