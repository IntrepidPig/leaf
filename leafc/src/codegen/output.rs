use std::io::{self, Write};

use leafvm::instruction::Instruction;

pub fn serialize_instructions<W: Write>(
	instructions: Vec<Instruction>,
	mut output: W,
) -> io::Result<()> {
	for instruction in instructions {
		match instruction {
			Instruction::Call(ptr, argc) => {
				write_val(2u8, &mut output, |t| vec![*t])?;
				write_val(ptr, &mut output, |t| usize_to_u8s(*t).to_vec())?;
				write_val(argc, &mut output, |t| usize_to_u8s(*t).to_vec())?;
			},
			Instruction::Block => write_val(3u8, &mut output, |t| vec![*t])?,
			Instruction::PushInt(val) => {
				write_val(4u8, &mut output, |t| vec![*t])?;
				write_val(val, &mut output, |t| u64_to_u8s(*t).to_vec())?;
			},
			Instruction::Output => write_val(5u8, &mut output, |t| vec![*t])?,
			Instruction::Bind => write_val(6u8, &mut output, |t| vec![*t])?,
			Instruction::Load(ptr) => {
				write_val(7u8, &mut output, |t| vec![*t])?;
				write_val(ptr, &mut output, |t| usize_to_u8s(*t).to_vec())?;
			},
			Instruction::Set(ptr) => {
				write_val(8u8, &mut output, |t| vec![*t])?;
				write_val(ptr, &mut output, |t| usize_to_u8s(*t).to_vec())?;
			},
			Instruction::Pop => write_val(9u8, &mut output, |t| vec![*t])?,
			Instruction::Add => write_val(10u8, &mut output, |t| vec![*t])?,
			Instruction::Sub => write_val(11u8, &mut output, |t| vec![*t])?,
			Instruction::Mul => write_val(12u8, &mut output, |t| vec![*t])?,
			Instruction::Div => write_val(13u8, &mut output, |t| vec![*t])?,
			Instruction::Debug => write_val(14u8, &mut output, |t| vec![*t])?,
			Instruction::Jump(ptr) => {
				write_val(15u8, &mut output, |t| vec![*t])?;
				write_val(ptr, &mut output, |t| usize_to_u8s(*t).to_vec())?;
			},
			Instruction::Check(ptr) => {
				write_val(16u8, &mut output, |t| vec![*t])?;
				write_val(ptr, &mut output, |t| usize_to_u8s(*t).to_vec())?;
			},
			Instruction::Equal => write_val(17u8, &mut output, |t| vec![*t])?,
			Instruction::Retrieve(ptr) => {
				write_val(18u8, &mut output, |t| vec![*t])?;
				write_val(ptr, &mut output, |t| usize_to_u8s(*t).to_vec())?;
			},
			Instruction::Return => write_val(19u8, &mut output, |t| vec![*t])?,
			Instruction::Ref(size) => {
				write_val(20u8, &mut output, |t| vec![*t])?;
				write_val(size, &mut output, |t| usize_to_u8s(*t).to_vec())?
			},
			Instruction::Terminate => write_val(21u8, &mut output, |t| vec![*t])?,
			Instruction::PushRoot => write_val(22u8, &mut output, |t| vec![*t])?,
			Instruction::PushBool(val) => {
				write_val(23u8, &mut output, |t| vec![*t])?;
				write_val(val, &mut output, |t| vec![*t as u8])?;
			},
		}
	}
	write_val(0u8, &mut output, |t| vec![*t])?;
	Ok(())
}

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn write_val<T, W: Write, F: FnMut(&T) -> Vec<u8>>(
	val: T,
	mut out: W,
	mut to_le: F,
) -> io::Result<()> {
	let size = ::std::mem::size_of::<T>();
	if size < 255 {
		out.write_all(&[size as u8])?;
	} else {
		out.write_all(&[255u8])?;
		out.write_all(&u64_to_u8s(size as u64))?; // might fail on >64 bit platforms
	}

	out.write_all(&to_le(&val))?;

	Ok(())
}

fn u64_to_u8s(num: u64) -> [u8; 8] {
	let mut buf = [0u8; 8];
	for (i, item) in buf.iter_mut().enumerate() {
		*item = ((num >> (i * 8)) & 0xffu64) as u8;
	}
	buf
}

fn usize_to_u8s(size: usize) -> [u8; 8] {
	u64_to_u8s(size as u64) // might fail on >64 bit platforms
}
