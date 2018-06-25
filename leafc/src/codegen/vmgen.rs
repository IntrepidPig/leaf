use ast::parser::{SyntaxTree, Block, Statement, Expression, Binding as SyntaxBinding};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
	val: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	Push(Value),
	Bind(String),
	PushVar(String),
	Drop(String),
	Debug,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	name: String,
	val: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator {
	blocks: Vec<Block>,
}

impl CodeGenerator {
	pub fn gen_instructions(ast: SyntaxTree) -> Vec<Instruction> {
		let instructions = CodeGenerator::gen_from_ast(&ast);

		instructions
	}

	pub fn gen_from_ast(ast: &SyntaxTree) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match ast {
			SyntaxTree::Block(ref block) => {
				instructions.append(&mut CodeGenerator::gen_from_block(block));		
			},
			SyntaxTree::Statement(ref stmnt) => {
				instructions.append(&mut CodeGenerator::gen_from_stmnt(stmnt));
			}
		}

		instructions
	}

	pub fn gen_from_block(block: &Block) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		for syntax in &block.block {
			instructions.append(&mut CodeGenerator::gen_from_ast(syntax))
		}

		instructions
	}

	pub fn gen_from_stmnt(stmnt: &Statement) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match stmnt {
			Statement::Binding(SyntaxBinding { ref ident, val: expr, .. }) => {
				if let Some(expr) = expr {
					instructions.append(&mut CodeGenerator::gen_from_expr(expr));
					instructions.push(Instruction::Bind(ident.to_owned()));
				} else {
					instructions.push(Instruction::Push(Value {
						val: 0,
					}));
					instructions.push(Instruction::Bind(ident.to_owned()));
				}
			},
			Statement::Debug(ref expr) => {
				instructions.append(&mut CodeGenerator::gen_from_expr(expr));
				instructions.push(Instruction::Debug)
			},
			Statement::Expression(ref expr) => {
				instructions.append(&mut CodeGenerator::gen_from_expr(expr));
			}
		}

		instructions
	}

	pub fn gen_from_expr(expr: &Expression) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match expr {
			Expression::NumberLiteral(num) => instructions.push(Instruction::Push(Value {
				val: *num,
			})),
			Expression::Block(ref block) => {
				instructions.append(&mut CodeGenerator::gen_from_block(block));
			}
			Expression::Identifier(ref ident) => {
				instructions.push(Instruction::PushVar(ident.to_owned()))
			},
			_ => unimplemented!()
		}

		instructions
	}
}