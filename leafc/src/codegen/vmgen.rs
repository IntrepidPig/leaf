use ast::parser::{SyntaxTree, Statement, Expression, Binding};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
	pub val: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	Push(Value),
	Pop,
	Bind(String),
	PushVar(String),
	Drop(String),
	Debug,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator {
	/// The stack currently as of the parsing stage. Each binding adds a variable to the last vec,
	/// each new block pushes a new vec, and when a block ends, a drop instruction for each binding
	/// in the last vec and then the last vec is popped. This creates a drop when variables go out
	/// of scope mechanism.
	block_vars: Vec<Vec<String>>,
}

impl CodeGenerator {
	pub fn new() -> Self {
		CodeGenerator {
			block_vars: vec![Vec::new()],
		}
	}

	pub fn gen_instructions(&mut self, ast: SyntaxTree) -> Vec<Instruction> {
		let instructions = self.gen_from_ast(&ast);

		instructions
	}

	pub fn gen_from_ast(&mut self, ast: &SyntaxTree) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		self.block_vars.push(Vec::new());
		for statement in &ast.block {
			instructions.append(&mut self.gen_from_stmnt(statement));
		}
		if let Some(ref output) = ast.output {
			instructions.append(&mut self.gen_from_expr(output))
		}
		for block_var in self.block_vars.pop().unwrap() {
			instructions.push(Instruction::Drop(block_var));
		}

		instructions
	}

	pub fn gen_from_stmnt(&mut self, stmnt: &Statement) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match stmnt {
			Statement::Binding(Binding { ref ident, val: expr, .. }) => {
				if let Some(expr) = expr {
					instructions.append(&mut self.gen_from_expr(expr));
					instructions.push(Instruction::Bind(ident.to_owned()));
				} else {
					instructions.push(Instruction::Push(Value {
						val: 0,
					}));
					instructions.push(Instruction::Bind(ident.to_owned()));
				}
				self.block_vars.last_mut().unwrap().push(ident.to_owned());
			},
			Statement::Debug(ref expr) => {
				instructions.append(&mut self.gen_from_expr(expr));
				instructions.push(Instruction::Debug)
			},
			Statement::Expression(ref expr) => {
				instructions.append(&mut self.gen_from_expr(expr));
				instructions.push(Instruction::Pop);
			}
		}

		instructions
	}

	pub fn gen_from_expr(&mut self, expr: &Expression) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match expr {
			Expression::NumberLiteral(num) => instructions.push(Instruction::Push(Value {
				val: *num,
			})),
			Expression::Block(ref ast) => {
				instructions.append(&mut self.gen_from_ast(ast));
			}
			Expression::Identifier(ref ident) => {
				instructions.push(Instruction::PushVar(ident.to_owned()))
			},
			_ => unimplemented!()
		}

		instructions
	}
}

pub struct ByteGen {

}

impl ByteGen {
	pub fn gen_bytes(_instructions: &[Instruction]) {
		unimplemented!()
	}
}