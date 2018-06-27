use ast::parser::{SyntaxTree, Statement, Expression, Binding, BinaryOp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
	pub val: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	Frame,
	Exit,
	Push(Value),
	Return,
	Bind(String),
	Load(String),
	Set(String),
	Pop,
	Add,
	Debug,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator {

}

impl CodeGenerator {
	pub fn new() -> Self {
		CodeGenerator {
			
		}
	}

	pub fn gen_instructions(&mut self, ast: SyntaxTree) -> Vec<Instruction> {
		let instructions = self.gen_from_ast(&ast);

		instructions
	}

	pub fn gen_from_ast(&mut self, ast: &SyntaxTree) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		instructions.push(Instruction::Frame);
		for statement in &ast.block {
			instructions.append(&mut self.gen_from_stmnt(statement));
		}
		if let Some(ref output) = ast.output {
			instructions.append(&mut self.gen_from_expr(output));
			instructions.push(Instruction::Return);
		}
		instructions.push(Instruction::Exit);

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
					unimplemented!();
				}
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
				instructions.push(Instruction::Load(ident.to_owned()))
			},
			Expression::Binary { left, right, op } => {
				instructions.append(&mut self.gen_from_expr(left));
				instructions.append(&mut self.gen_from_expr(right));
				instructions.push(match op {
					BinaryOp::Add => Instruction::Add,
				});
			}
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