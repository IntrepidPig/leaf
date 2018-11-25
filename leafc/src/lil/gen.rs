use ::lil::*;
use ::lil::instruction::*;
use ::hir::{self, HIR};
use ::ast::parser::{ModulePath};

pub struct LILGenerator<'a> {
	hir: &'a HIR
}

impl<'a> LILGenerator<'a> {
	pub fn new(hir: &'a HIR) -> Self {
		LILGenerator {
			hir,
		}
	}

	pub fn gen_lil(&mut self) -> Result<LIL, LILGenError> {
		let mut functiondefs: Vec<FunctionDef> = Vec::new();
		let mut fnfailures: Vec<LILGenError> = Vec::new();

		self.hir.modules[0].traverse(&mut |module_path: &ModulePath, module: &hir::Module| {
			for hfn in &module.functions {
				match self.gen_lil_function(hfn) {
					Ok(f) => functiondefs.push(f),
					Err(e) => fnfailures.push(e),
				}
			}
		}, &mut ModulePath::new(false, Vec::new()));

		let mut last = None;

		for failure in fnfailures {
			println!("{:?}", failure);
			last = Some(failure);
		}

		if let Some(err) = last {
			return Err(err);
		}

		Ok(LIL {
			typedefs: Vec::new(),
			functiondefs,
		})
	}

	pub fn gen_lil_function(&mut self, hfn: &hir::FunctionDefinition) -> Result<FunctionDef, LILGenError> {
		println!("Gen from {:?}", hfn);
		let mut blocks: Vec<Block> = Vec::new();
		let mut root_block: Block = Block {
			statements: Vec::new(),
			terminator: None,
		};

		let mut ctx = FunctionContext {
			locals: Vec::new(),
		};

		hfn.body.traverse(&mut |expr: &hir::Expression| {
			match expr.expr {
				hir::ExpressionType::Binding(ref binding) => {
					ctx.locals.push(Identifier {
						ident: binding.ident.name.clone(),
					});
				},
				_ => {},
			}
		});

		println!("{:?}", hfn);
		self.gen_from_expr(&ctx, &mut root_block, &mut blocks, &hfn.body)?;
		println!("{:?}", root_block);

		Ok(FunctionDef {
			locals: ctx.locals.len(),
			root_block,
			blocks,
		})
	}

	pub fn gen_from_expr(&mut self, ctx: &FunctionContext, current: &mut Block, blocks: &mut Vec<Block>, expr: &hir::Expression) -> Result<(), LILGenError> {
		println!("Gen from expr {:?}", expr);

		match expr.expr {
			hir::ExpressionType::Root => {
				current.statements.push(Statement::Instruction(LILInstruction::PushRoot));
			},
			hir::ExpressionType::FunctionCall(ref call) => unimplemented!(),
			hir::ExpressionType::Debug(ref debug) => unimplemented!(),
			hir::ExpressionType::Binding(ref binding) => unimplemented!(),
			hir::ExpressionType::Assignment(ref assignment) => {
				self.gen_from_expr(ctx, current, blocks, &assignment.expr)?;
				current.statements.push(Statement::Assignment(Assignment {
					target: Storage::Local(Local { index: ctx.locals.iter().position(|x| x.ident == assignment.ident.name).unwrap() }),
				}));
			},
			hir::ExpressionType::IntLiteral(ref val) => {
				current.statements.push(Statement::Instruction(LILInstruction::PushInt(*val as i32)));
			},
			hir::ExpressionType::BoolLiteral(ref val) => unimplemented!(),
			hir::ExpressionType::Block(ref block) => {
				for statement in &block.statements {
					self.gen_from_expr(ctx, current, blocks, statement)?;
					current.statements.push(Statement::Instruction(LILInstruction::Pop));
				}
				if let Some(ref terminator) = block.output {
					self.gen_from_expr(ctx, current, blocks, terminator)?;
				}
			},
			hir::ExpressionType::Binary(ref bin) => {
				self.gen_from_expr(ctx, current, blocks, &bin.left)?;
				self.gen_from_expr(ctx, current, blocks, &bin.right)?;

				use ::ast::parser::BinaryOp;
				match bin.op {
					BinaryOp::Equality => {
						current.statements.push(Statement::Instruction(LILInstruction::Equal));
					},
					_ => unimplemented!(),
				}
			},
			_ => unimplemented!(),
		};

		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionContext {
	locals: Vec<Identifier>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LILGenError {
	msg: String,
}

/*impl<T: ::std::fmt::Debug> From<T> for LILGenError {
	fn from(t: T) -> Self {
		LILGenError {
			msg: format!("{:?}", t),
		}
	}
}*/

/*impl<T: ::std::fmt::Display> From<T> for LILGenError {
	fn from(t: T) -> Self {
		LILGenError {
			msg: format!("{}", t),
		}
	}
}*/

impl ::std::fmt::Display for LILGenError {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(f, "{}", self.msg)
	}
}

impl ::std::error::Error for LILGenError { }

pub fn gen_lil(hir: &HIR) -> Result<LIL, LILGenError> {
	let mut lilgenerator = LILGenerator::new(hir);

	lilgenerator.gen_lil()
}

pub fn gen_test_lil() -> LIL {
	LIL {
		typedefs: Vec::new(),
		functiondefs: vec![
			FunctionDef {
				locals: 2,
				root_block: Block {
					statements: vec![
						Statement::Instruction(LILInstruction::PushBool(true)),
						Statement::Instruction(LILInstruction::PushInt(4)),
						Statement::Instruction(LILInstruction::Bind),
						Statement::Instruction(LILInstruction::Bind),
						Statement::Instruction(LILInstruction::PushInt(14)),
						Statement::Assignment(Assignment {
							target: Storage::Local(Local { index: 0, }),
						}),
						Statement::Instruction(LILInstruction::PushBool(false)),
						Statement::Assignment(Assignment {
							target: Storage::Local(Local { index: 1, }),
						}),
					],
					terminator: None,
				},
				blocks: Vec::new(),
			}
		],
	}
}