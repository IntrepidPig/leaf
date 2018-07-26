use std::io::{self, Write};

use leafvm::instruction::Instruction;

pub fn serialize_instructions<W: Write>(
	instructions: Vec<Instruction>,
	mut output: W,
) -> io::Result<()> {
	for instruction in instructions {
		match instruction {
			Instruction::Call(ptr, argc) => {
				write_val(&2u8, &mut output)?;
				write_val(&ptr, &mut output)?;
				write_val(&argc, &mut output)?;
			},
			Instruction::Block => write_val(&3u8, &mut output)?,
			Instruction::PushInt(val) => {
				write_val(&4u8, &mut output)?;
				write_val(&val, &mut output)?;
			},
			Instruction::Output => write_val(&5u8, &mut output)?,
			Instruction::Bind => write_val(&6u8, &mut output)?,
			Instruction::Load(ptr) => {
				write_val(&7u8, &mut output)?;
				write_val(&ptr, &mut output)?;
			},
			Instruction::Set(ptr) => {
				write_val(&8u8, &mut output)?;
				write_val(&ptr, &mut output)?;
			},
			Instruction::Pop => write_val(&9u8, &mut output)?,
			Instruction::Add => write_val(&10u8, &mut output)?,
			Instruction::Sub => write_val(&11u8, &mut output)?,
			Instruction::Mul => write_val(&12u8, &mut output)?,
			Instruction::Div => write_val(&13u8, &mut output)?,
			Instruction::Debug => write_val(&14u8, &mut output)?,
			Instruction::Jump(ptr) => {
				write_val(&15u8, &mut output)?;
				write_val(&ptr, &mut output)?;
			},
			Instruction::Check(ptr) => {
				write_val(&16u8, &mut output)?;
				write_val(&ptr, &mut output)?;
			},
			Instruction::Equal => write_val(&17u8, &mut output)?,
			Instruction::Retrieve(ptr) => {
				write_val(&18u8, &mut output)?;
				write_val(&ptr, &mut output)?;
			},
			Instruction::Return => write_val(&19u8, &mut output)?,
			Instruction::Ref(size) => {
				write_val(&20u8, &mut output)?;
				write_val(&size, &mut output)?
			},
			Instruction::Terminate => write_val(&21u8, &mut output)?,
			Instruction::PushRoot => write_val(&22u8, &mut output)?,
			Instruction::PushBool(val) => {
				write_val(&23u8, &mut output)?;
				write_val(&val, &mut output)?;
			},
		}
	}
	write_val(&0u8, &mut output)?;
	Ok(())
}

fn write_val<T: ToBytesLe, W: Write>(val: &T, mut out: W) -> io::Result<()> {
	let bytes = val.to_bytes_le();
	let size = bytes.len();
	if size < 255 {
		out.write_all(&[size as u8])?;
	} else {
		out.write_all(&[255u8])?;
		out.write_all(&size.to_bytes_le())?;
	}

	out.write_all(&bytes)?;

	Ok(())
}

// Trait to represent a type that can be converted to bytes, in little endian if necessary
trait ToBytesLe {
	fn to_bytes_le(&self) -> Vec<u8>;
}

impl ToBytesLe for u8 {
	fn to_bytes_le(&self) -> Vec<u8> {
		vec![*self]
	}
}

impl ToBytesLe for u64 {
	fn to_bytes_le(&self) -> Vec<u8> {
		let mut buf = [0u8; 8];
		for (i, item) in buf.iter_mut().enumerate() {
			*item = ((*self >> (i * 8)) & 0xffu64) as u8;
		}
		buf.to_vec()
	}
}

impl ToBytesLe for usize {
	fn to_bytes_le(&self) -> Vec<u8> {
		let size = ::std::mem::size_of::<Self>();
		let mut buf: Vec<u8> = Vec::with_capacity(size);
		for i in 0..size {
			buf.push(((*self >> (i * 8)) & 0xffusize) as u8);
		}
		buf
	}
}

impl ToBytesLe for bool {
	fn to_bytes_le(&self) -> Vec<u8> {
		vec![*self as u8]
	}
}
