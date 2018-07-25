pub mod vmgen;

use std::io::Write;

use leafvm::instruction::Instruction;

pub fn serialize_instructions(instructions: Vec<Instruction>, output: &mut Write) {
	for instruction in instructions {
		let buf = match instruction {
			Instruction::Call(ptr, argc) => to_u8_le(&[2, ptr, argc]),
			Instruction::Block => to_u8_le(&[3]),
			Instruction::PushInt(val) => to_u8_le(&[4, val as usize]),
			Instruction::Output => to_u8_le(&[5]),
			Instruction::Bind => to_u8_le(&[6]),
			Instruction::Load(ptr) => to_u8_le(&[7, ptr]),
			Instruction::Set(ptr) => to_u8_le(&[8, ptr]),
			Instruction::Pop => to_u8_le(&[9]),
			Instruction::Add => to_u8_le(&[10]),
			Instruction::Sub => to_u8_le(&[11]),
			Instruction::Mul => to_u8_le(&[12]),
			Instruction::Div => to_u8_le(&[13]),
			Instruction::Debug => to_u8_le(&[14]),
			Instruction::Jump(ptr) => to_u8_le(&[15, ptr]),
			Instruction::Check(ptr) => to_u8_le(&[16, ptr]),
			Instruction::Equal => to_u8_le(&[17]),
			Instruction::Retrieve(ptr) => to_u8_le(&[18, ptr]),
			Instruction::Return => to_u8_le(&[19]),
			Instruction::Ref(size) => to_u8_le(&[20, size]),
			Instruction::Terminate => to_u8_le(&[21]),
			Instruction::PushRoot => to_u8_le(&[22]),
			Instruction::PushBool(val) => to_u8_le(&[23, val as usize]),
		};
		output.write_all(&buf).unwrap();
	}
}

fn to_u8_le(sizes: &[usize]) -> Vec<u8> {
	let mut buf = Vec::new();
	for size in sizes {
		for i in 0usize..8usize {
			buf.push(((size >> (i * 8)) & 0xffusize) as u8);
		}
	}
	buf
}
