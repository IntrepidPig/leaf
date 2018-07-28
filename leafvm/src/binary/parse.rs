use std::io::{self, Read};
use instruction::Instruction;
use binary::Binary;

pub fn read_item<R: Read>(mut input: R) -> io::Result<Vec<u8>> {
	let mut len = [0u8; 1];
	input.read_exact(&mut len)?;
	let len = len[0];
	let len: u64 = if len == 255 {
		let mut long_len = [0u8; 8];
		input.read_exact(&mut long_len)?;
		u8s_to_u64_le(&long_len)
	} else {
		u64::from(len)
	};
	let mut buf: Vec<u8> = Vec::with_capacity(len as usize);
	let mut handle = input.take(len);
	handle.read_to_end(&mut buf)?;
	Ok(buf)
}

fn u8s_to_u64_le(input: &[u8]) -> u64 {
	if input.len() > 8 {
		panic!("Tried to convert a slice of u8s to a u64 with more than 8 elements")
	}
	let mut val = 0u64;
	for (i, item) in input.iter().enumerate() {
		val += u64::from(*item) * 256u64.pow(i as u32);
	}
	val
}

fn next_u64<R: Read>(mut input: R) -> io::Result<u64> {
	let num_bytes = read_item(&mut input)?;
	let num = u8s_to_u64_le(&num_bytes);
	Ok(num)
}

fn next_usize<R: Read>(mut input: R) -> io::Result<usize> {
	let num_bytes = read_item(&mut input)?;
	if num_bytes.len() > ::std::mem::size_of::<usize>() || num_bytes.len() > 8 {
		panic!("Tried to load a pointer that was larger than this systems address size")
	}
	let num = u8s_to_u64_le(&num_bytes);

	Ok(num as usize)
}

pub fn read_instructions<R: Read>(mut raw: R) -> io::Result<Vec<Instruction>> {
	let mut instructions = Vec::new();
	loop {
		let opcode = next_u64(&mut raw)?;
		let instruction = match opcode {
			0 => break,
			2 => Instruction::Call(next_usize(&mut raw)?, next_usize(&mut raw)?),
			3 => Instruction::Block,
			4 => Instruction::PushInt(next_u64(&mut raw)?),
			5 => Instruction::Output,
			6 => Instruction::Bind,
			7 => Instruction::Load(next_usize(&mut raw)?),
			8 => Instruction::Set(next_usize(&mut raw)?),
			9 => Instruction::Pop,
			10 => Instruction::Add,
			11 => Instruction::Sub,
			12 => Instruction::Mul,
			13 => Instruction::Div,
			14 => Instruction::Debug,
			15 => Instruction::Jump(next_usize(&mut raw)?),
			16 => Instruction::Check(next_usize(&mut raw)?),
			17 => Instruction::Equal,
			18 => Instruction::Retrieve(next_usize(&mut raw)?),
			19 => Instruction::Return,
			20 => Instruction::Ref(next_usize(&mut raw)?),
			21 => Instruction::Terminate,
			22 => Instruction::PushRoot,
			23 => Instruction::PushBool(next_usize(&mut raw)? != 0),
			24 => Instruction::ExternCall(
				next_usize(&mut raw)?,
				next_usize(&mut raw)?,
				next_usize(&mut raw)?,
			),
			_ => panic!("Unknown opcode {}", opcode),
		};
		instructions.push(instruction);
	}
	Ok(instructions)
}

pub fn read_externs<R: Read>(mut raw: R) -> io::Result<Vec<String>> {
	let mut externs = Vec::new();
	while let Ok(item) = read_item(&mut raw) {
		externs.push(String::from_utf8(item).unwrap())
	}
	Ok(externs)
}

pub fn read_binary<R: Read>(mut raw: R) -> io::Result<Binary> {
	let _symbol_table = read_item(&mut raw)?;
	let extern_table = read_item(&mut raw)?;
	let externs = read_externs(extern_table.as_slice())?;
	let instruction_table = read_item(&mut raw)?;
	let instructions = read_instructions(instruction_table.as_slice())?;

	Ok(Binary {
		symbol_table: Vec::new(),
		extern_table: externs,
		instructions,
	})
}

pub fn print_instructions(instructions: &[Instruction]) {
	let max_length = instructions.len().to_string().len();
	for (i, instr) in instructions.iter().enumerate() {
		let i_str = i.to_string();
		for _ in 0..max_length - i_str.len() {
			eprint!(" ");
		}
		eprint!("{}: {:?}\n", i_str, instr);
	}
}

pub fn print_bin(bin: &Binary) {
	eprintln!("External Functions:\n-------------------");
	for extern_fn in &bin.extern_table {
		eprintln!("{}", extern_fn);
	}
	eprintln!("Instructions:\n-------------");
	print_instructions(&bin.instructions);
}
