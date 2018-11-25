use std::collections::HashMap;

use ::lil::*;
use ::lil::instruction::*;
use ast::parser::operators::*;
use ast::parser::{Identifier, ModulePath, PathItem, TypeName};
use leafvm::instruction::Instruction;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeGenerator<'a> {
	lil: &'a LIL,
	pub instructions: Vec<Instruction>,
	function_locations: Vec<(usize, usize)>,
	function_jumps_todo: Vec<(usize, usize)>,
}

pub struct LIR {
	pub symbol_table: Vec<()>,
	pub extern_table: Vec<String>,
	pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LIRGenError {
	msg: String,
}

impl<'a> CodeGenerator<'a> {
	pub fn new(lil: &'a LIL) -> Self {
		CodeGenerator {
			lil,
			instructions: Vec::new(),
			function_locations: Vec::new(),
			function_jumps_todo: Vec::new(),
		}
	}

	pub fn gen_lir(&mut self) -> Result<LIR, LIRGenError> {
		for (id, function) in self.lil.functiondefs.iter().enumerate() {
			self.gen_from_function(id, function);
		}

		let lir = LIR {
			symbol_table: Vec::new(),
			extern_table: Vec::new(),
			instructions: self.instructions.clone(),
		};

		Ok(lir)
	}

	fn gen_from_function(&mut self, id: usize, function: &FunctionDef) {
		self.function_locations.push((id, self.instructions.len()));
		for statement in &function.root_block.statements {
			match statement {
				Statement::Instruction(ref instruction) => {
					self.gen_from_instruction(instruction);
				}
				Statement::Assignment(ref assignment) => {
					self.gen_from_assignment(assignment);
				},
				Statement::Call(ref call) => {
					unimplemented!()
				},
			}
		}
	}

	fn gen_from_assignment(&mut self, assignment: &Assignment) {
		match assignment.target {
			Storage::Local(Local { index, }) => {
				self.instructions.push(Instruction::Set(index));
			}
		}
	}

	fn gen_from_instruction(&mut self, instruction: &LILInstruction) {
		self.instructions.push(match instruction {
			LILInstruction::Set(ref idx) => Instruction::Set(*idx),
			LILInstruction::PushInt(ref val) => Instruction::PushInt(*val as u64),
			LILInstruction::PushBool(ref val) => Instruction::PushBool(*val),
			LILInstruction::Pop => Instruction::Pop,
			LILInstruction::Bind => Instruction::Bind,
			LILInstruction::Equal => Instruction::Equal,
			_ => unimplemented!(),
		})
	}
}