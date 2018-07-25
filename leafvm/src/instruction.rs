use std::io::Read;

/// An instruction/opcode for the vm
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	/// Call a function that starts at usize and create a new stack frame. Pop a
	//. certain amount of values from the current stack frame into the next (arguments)
	Call(usize, usize),
	/// New block frame
	Block,
	/// Exit the stack frame, dropping all the values it had
	PushInt(u64),
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
	/// Pop the top two values on the stack and push their difference back
	Sub,
	/// Pop the top two values on the stack and push their product back
	Mul,
	/// Pop the top two values on the stack and push their quotient back
	Div,
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
	Terminate,
	/// Push a Root value onto the operand stack
	PushRoot,
	/// Push a boolean onto the operand stack
	PushBool(bool),
}

pub fn print_instructions(instructions: &[Instruction]) {
	let max_length = instructions.len().to_string().len();
	for (i, instr) in instructions.iter().enumerate() {
		let i_str = i.to_string();
		for _ in 0..max_length - i_str.len() {
			print!(" ");
		}
		println!("{}: {:?}", i_str, instr);
	}
}

pub fn read_instructions<R: Read>(mut raw: R) -> Vec<Instruction> {
	let mut out = Vec::<usize>::new();
	let mut buf = [0u8; 8];
	while let Ok(_) = raw.read_exact(&mut buf) {
		let mut val: usize = 0;
		for (i, item) in buf.iter().enumerate() {
			val += *item as usize * 256usize.pow(i as u32);
		}
		out.push(val);
	}
	parse_instructions(out)
}

pub fn parse_instructions(mut raw: Vec<usize>) -> Vec<Instruction> {
	let mut instructions: Vec<Instruction> = Vec::new();
	while !raw.is_empty() {
		let opcode = raw.remove(0);
		instructions.push(match opcode {
			2 => Instruction::Call(raw.remove(0), raw.remove(0)),
			3 => Instruction::Block,
			4 => Instruction::PushInt(raw.remove(0) as u64),
			5 => Instruction::Output,
			6 => Instruction::Bind,
			7 => Instruction::Load(raw.remove(0)),
			8 => Instruction::Set(raw.remove(0)),
			9 => Instruction::Pop,
			10 => Instruction::Add,
			11 => Instruction::Sub,
			12 => Instruction::Mul,
			13 => Instruction::Div,
			14 => Instruction::Debug,
			15 => Instruction::Jump(raw.remove(0)),
			16 => Instruction::Check(raw.remove(0)),
			17 => Instruction::Equal,
			18 => Instruction::Retrieve(raw.remove(0)),
			19 => Instruction::Return,
			20 => Instruction::Ref(raw.remove(0)),
			21 => Instruction::Terminate,
			22 => Instruction::PushRoot,
			23 => Instruction::PushBool(raw.remove(0) != 0),
			_ => panic!("Unknown opcode {}", opcode),
		});
	}
	instructions
}
