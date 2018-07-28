pub mod parse;

use instruction::Instruction;

pub struct Binary {
	pub symbol_table: Vec<String>,
	pub extern_table: Vec<String>,
	pub instructions: Vec<Instruction>,
}

pub struct Library {}
