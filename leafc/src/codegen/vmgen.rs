use std::collections::HashMap;

use hir::*;
use ast::parser::operators::*;
use ast::parser::{Identifier, ModulePath, PathItem, TypeName};
use leafvm::instruction::Instruction;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator<'a> {
	hir: &'a HIR,
	root_mod: &'a Module,
	pub instructions: Vec<Instruction>,
	loop_breaks: Vec<Vec<usize>>,
	block_is_loop: bool,
	if_jumps: Vec<usize>,
	locals: Vec<Vec<Identifier>>,
	function_locations: HashMap<PathItem<Identifier>, usize>,
	function_jumps_todo: Vec<(usize, PathItem<Identifier>)>,
	module_path: ModulePath,
}

pub struct LIR {
	pub symbol_table: Vec<()>,
	pub extern_table: Vec<String>,
	pub instructions: Vec<Instruction>,
}

impl<'a> CodeGenerator<'a> {
	pub fn new(hir: &'a HIR) -> Self {
		CodeGenerator {
			hir,
			root_mod: &hir.modules[0],
			instructions: Vec::new(),
			loop_breaks: Vec::new(),
			block_is_loop: false,
			if_jumps: Vec::new(),
			locals: vec![Vec::new()],
			function_locations: HashMap::new(),
			function_jumps_todo: Vec::new(),
			module_path: ModulePath::new(false, Vec::new()),
		}
	}

	pub fn gen_lir(&mut self) -> LIR {
		self.gen_instructions();
		let mut extern_table = Vec::new();
		for extern_fn in &self.hir.extern_table {
			extern_table.push(extern_fn.name.to_string());
		}
		LIR {
			symbol_table: Vec::new(),
			extern_table,
			instructions: self.instructions.clone(),
		}
	}

	pub fn find_typedef(&self, path: &PathItem<TypeName>) -> Option<TypeDefinition> {
		let mut typedef = None;
		self.root_mod.traverse(
			&mut |current_path: &ModulePath, module: &Module| {
				for test_typedef in &module.types {
					if test_typedef.name == path.item && current_path == &path.module_path {
						typedef = Some(test_typedef.clone());
					}
				}
			},
			&mut ModulePath::new(false, Vec::new()),
		);

		typedef
	}

	pub fn find_function(&self, path: &PathItem<Identifier>) -> Option<FunctionDefinition> {
		let mut functiondef = None;
		self.root_mod.traverse(
			&mut |current_path: &ModulePath, module: &Module| {
				for test_function in &module.functions {
					if test_function.name == path.item && current_path == &path.module_path {
						functiondef = Some(test_function.clone());
					}
				}
			},
			&mut ModulePath::new(false, Vec::new()),
		);

		functiondef
	}

	pub fn gen_instructions(&mut self) {
		self.instructions.push(Instruction::Call(0, 0));
		self.function_jumps_todo
			.push((0, PathItem::root_item(Identifier::try_from_str("main"))));
		self.instructions.push(Instruction::Terminate);
		self.root_mod.traverse(
			&mut |module_path: &ModulePath, module: &Module| {
				for function in &module.functions {
					self.gen_from_func(function, module_path)
				}
			},
			&mut ModulePath::new(false, Vec::new()),
		);

		self.change_function_jumps();
	}

	pub fn change_function_jumps(&mut self) {
		for (location, name) in &self.function_jumps_todo {
			match self.instructions[*location] {
				Instruction::Call(ref mut target, _argc) => *target = self.function_locations[name],
				Instruction::ExternCall(ref mut lib_idx, ref mut symbol_idx, _argc) => {
					*lib_idx = 0; // In this binary only for now
					*symbol_idx = self.hir
						.extern_table
						.iter()
						.position(|x| x.name == *name)
						.unwrap();
				},
				_ => panic!("Expected a jump to {:?} at index {}", name, location),
			}
		}
	}

	pub fn gen_from_func(&mut self, function: &FunctionDefinition, module_path: &ModulePath) {
		self.locals.push(Vec::new());
		self.function_locations.insert(
			PathItem {
				module_path: module_path.clone(),
				item: function.name.clone(),
			},
			self.instructions.len(),
		);
		// start a new stack frame
		self.instructions.push(Instruction::Block);
		// load all of the arguments
		for arg in &function.args {
			self.locals.last_mut().unwrap().push(arg.0.clone());
		}
		// TODO bind arguments
		self.gen_from_expr(&function.body);
		// exit stack frame, return value at the top of the stack
		self.instructions.push(Instruction::Return);
		self.locals.pop();
	}

	/// Generate instructions for a block
	pub fn gen_from_block(&mut self, block: &Block) {
		// Start a new stack frame
		self.instructions.push(Instruction::Block);
		// Generate instructions for each statement in the block
		for statement in &block.statements {
			self.gen_from_expr(statement);
			// Pop the unused result of the expression
			self.instructions.push(Instruction::Pop);
		}
		// Generate instructions for the block output (if there is one)
		if let Some(ref output) = block.output {
			// Load the value of the exression onto the stack
			self.gen_from_expr(output);
		} else {
			// Return a nil value
			// This is necessary because if the block was a statement then it will always drop the result
			self.instructions.push(Instruction::PushRoot);
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
			self.gen_from_expr(&else_block);
		} else {
			self.instructions.push(Instruction::PushRoot);
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
		match expr.expr {
			// Push literal values onto the stack
			ExpressionType::IntLiteral(num) => self.instructions.push(Instruction::PushInt(num)),
			// Generate instructions for nested blocks
			ExpressionType::Block(ref ast) => {
				self.gen_from_block(ast);
			},
			// Dereference variables
			ExpressionType::Identifier(ref ident) => {
				// Push the value of the variable onto the top of the stack
				self.instructions.push(Instruction::Load(
					self.locals
						.last()
						.unwrap()
						.iter()
						.position(|test_name| test_name == ident)
						.unwrap(),
				))
			},
			// Evalute binary expressions
			ExpressionType::Binary(ref binary) => {
				// Push the value of the left hand side to the stack
				self.gen_from_expr(&binary.left);
				// Push the value of the right hand side to the stack
				self.gen_from_expr(&binary.right);
				// Perform the operation
				match binary.op {
					BinaryOp::Add => self.instructions.push(Instruction::Add),
					BinaryOp::Sub => self.instructions.push(Instruction::Sub),
					BinaryOp::Mul => self.instructions.push(Instruction::Mul),
					BinaryOp::Div => self.instructions.push(Instruction::Div),
					BinaryOp::Equality => self.instructions.push(Instruction::Equal),
					BinaryOp::Assign => panic!(
						"Assign binary operator found. This should have been converted to a standalone\
						 expression during the parsing phase, not a binary operation"
					),
					_ => unimplemented!(),
				};
			},
			ExpressionType::Assignment(ref assignment) => {
				// Load the value that used to be in the variable being assigned to
				self.instructions.push(Instruction::Load(
					self.locals
						.last()
						.unwrap()
						.iter()
						.position(|test_name| test_name == &assignment.ident)
						.unwrap(),
				));
				// Push the result of the expr onto the stack
				self.gen_from_expr(&assignment.expr);
				// Move the result into the variable
				self.instructions.push(Instruction::Set(
					self.locals
						.last()
						.unwrap()
						.iter()
						.position(|test_name| test_name == &assignment.ident)
						.unwrap(),
				));
				// And the old value will be left on the stack as the result of the expression
			},
			ExpressionType::Loop(ref block) => {
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
			ExpressionType::If(ref if_stmnt) => {
				// Gen the instructions for the if statement
				self.gen_from_if(if_stmnt);
			},
			ExpressionType::Binding(ref binding) => {
				// If there's an expression
				if let Some(ref expr) = binding.expr {
					// Generate the instructions from the expression
					// This will push the result onto the stack
					self.gen_from_expr(expr);
					// Bind the variable to the current local var location
					self.instructions.push(Instruction::Bind);
					// Push the variable name to the local var stack
					self.locals.last_mut().unwrap().push(binding.ident.clone())
				} else {
					// Push a nil value to the stack
					self.instructions.push(Instruction::PushRoot);
					// Bind the variable to the nil value
					self.instructions.push(Instruction::Bind);
					// Push the variable name to the local var stack
					self.locals.last_mut().unwrap().push(binding.ident.clone());
				}
				// Push a nil value since let is an expression and it's result will be popped
				self.instructions.push(Instruction::PushRoot)
			},
			ExpressionType::Debug(ref expr) => {
				// Generate the expression instructions
				self.gen_from_expr(expr);
				// Debug the value at the top of the stack (pops it automatically)
				self.instructions.push(Instruction::Debug);
				// Push a nil value since debug is an expression and it's result will be popped
				self.instructions.push(Instruction::PushRoot)
			},
			ExpressionType::Break(ref expr) => {
				// If there's an associated expression then gen instructions for it, otherwise just push a nil
				if let Some(expr) = expr {
					self.gen_from_expr(expr);
				} else {
					self.instructions.push(Instruction::PushRoot)
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
				self.instructions.push(Instruction::PushRoot)
			},
			ExpressionType::BoolLiteral(val) => self.instructions.push(Instruction::PushBool(val)),
			ExpressionType::FunctionCall(ref call) => {
				for arg in &call.args {
					self.gen_from_expr(arg);
				}
				let function_call_index = self.instructions.len();
				self.function_jumps_todo
					.push((function_call_index, call.name.clone()));
				let is_extern = self.hir.extern_table.iter().any(|x| x.name == call.name);
				self.instructions.push(if is_extern {
					Instruction::ExternCall(0, 0, call.args.len())
				} else {
					Instruction::Call(0, call.args.len())
				});
			},
			ExpressionType::FieldAccess(ref expr, ref fieldname) => {
				self.gen_from_expr(expr);
				let exprtype = &expr.expr_out;
				let typedef = self.find_typedef(exprtype)
					.expect("Type not found in list of typedefs");
				let typeindex = typedef
					.fields
					.iter()
					.position(|field| &field.0 == fieldname)
					.expect("Type does not have this field");
				self.instructions.push(Instruction::Retrieve(typeindex));
			},
			ExpressionType::Instantiation(ref instantiation) => {
				let typedef = self.find_typedef(&instantiation.typename).unwrap().clone();
				if instantiation.fields.len() != typedef.fields.len() {
					panic!("Incorrect amount of fields provided for type")
				}
				for i in 0..instantiation.fields.len() {
					self.gen_from_expr(&instantiation.fields[i].1);
				}
				self.instructions
					.push(Instruction::Ref(instantiation.fields.len()))
			},
			ExpressionType::Root => self.instructions.push(Instruction::PushRoot),
			ExpressionType::StringLiteral(_) => unimplemented!(),
			ExpressionType::Postfix(ref postfix) => {
				self.gen_from_expr(&postfix.left);
				match postfix.op {
					_ => unimplemented!(),
				}
			},
			ExpressionType::Prefix(ref prefix) => {
				self.gen_from_expr(&prefix.right);
				match prefix.op {
					_ => unimplemented!(),
				}
			},
		}
	}
}
