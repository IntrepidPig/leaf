pub mod parse;

use instruction::Instruction;

pub struct Binary {
	pub link_table: Vec<String>,
	pub extern_table: Vec<String>,
	pub instructions: Vec<Instruction>,
}

pub struct Library {}
