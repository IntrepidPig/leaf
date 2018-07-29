use std::io::{self, Write};

use codegen::vmgen::LIR;

use leafvm::instruction::Instruction;

pub fn serialize_lir_bin<W: Write>(lir: &LIR, mut output: W) -> io::Result<()> {
	// Link table
	write_val(&0u8, &mut output)?;
	// Extern table
	let mut extern_table: Vec<u8> = Vec::new();
	serialize_externs(&lir.extern_table, &mut extern_table)?;
	write_val(&extern_table, &mut output)?;
	// Instruction table
	let mut instruction_table: Vec<u8> = Vec::new();
	serialize_instructions(lir.instructions.clone(), &mut instruction_table)?;
	write_val(&instruction_table, &mut output)?;

	Ok(())
}

pub fn serialize_externs<W: Write>(externs: &[String], mut output: W) -> io::Result<()> {
	for extern_item in externs {
		write_val(extern_item, &mut output)?;
	}

	Ok(())
}

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
			Instruction::ExternCall(lib_idx, symbol_idx, argc) => {
				write_val(&24u8, &mut output)?;
				write_val(&lib_idx, &mut output)?;
				write_val(&symbol_idx, &mut output)?;
				write_val(&argc, &mut output)?;
			},
		}
	}
	write_val(&0u8, &mut output)?;
	Ok(())
}

fn write_val<T: ToBytesLe, W: Write>(val: &T, out: &mut W) -> io::Result<()> {
	let bytes = val.to_bytes_le();
	let size = bytes.len();
	if size < 255 {
		out.write_all(&[size as u8])?;
	} else {
		out.write_all(&[255u8])?;
		write_val(&size, out)?;
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
		// Cut off exmpty bytes at the end of usize for space effieciency
		let mut size = 0;
		for (i, item) in buf.iter().enumerate() {
			if *item > 0 {
				size = i;
			}
		}
		size += 1;
		buf.split_off(::std::cmp::max(size, 1));
		buf
	}
}

impl ToBytesLe for bool {
	fn to_bytes_le(&self) -> Vec<u8> {
		vec![*self as u8]
	}
}

impl ToBytesLe for String {
	fn to_bytes_le(&self) -> Vec<u8> {
		self.as_bytes().to_vec()
	}
}

impl ToBytesLe for Vec<u8> {
	fn to_bytes_le(&self) -> Vec<u8> {
		self.clone()
	}
}
