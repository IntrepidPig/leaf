extern crate leafc;

#[cfg(test)]
mod tests;

use leafc::codegen::vmgen::{Instruction, Var, VarInfo, Primitive, Reference};

pub fn leaf() {
	println!("Running leaf");
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackFrame {
	locals: Vec<Var>,
	block_frames: Vec<BlockFrame>,
	return_to_ptr: usize,
}

impl StackFrame {
	pub fn new(return_to_ptr: usize) -> Self {
		StackFrame {
			locals: Vec::new(),
			block_frames: Vec::new(),
			return_to_ptr,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockFrame {
	operands: Vec<Var>,
}

impl BlockFrame {
	pub fn new() -> Self {
		BlockFrame {
			operands: Vec::new(),
		}
	}
}

pub fn run_instructions(instructions: &[Instruction], debug: bool) -> Result<Var, ()> {
	let mut ptr: usize = 0;
	// Create a stack with a main stack frame and block frame for the main function's outputs
	let mut stack: Vec<StackFrame> = vec![StackFrame::new(0)];
	stack[0].block_frames.push(BlockFrame::new());

	let mut instr_ptr: usize = 0;
	let mut iter: usize = 0;

	loop {
		if instr_ptr == instructions.len() {
			break;
		}

		if debug {
			iter += 1;
			if iter > 1000 {
				println!("Reached maximum instruction execution");
				return Err(());
			}

			println!("Instruction {}: {:?}", instr_ptr, instructions[instr_ptr],);
		}

		match instructions[instr_ptr] {
			Instruction::Block => {
				stack.last_mut().unwrap().block_frames.push(BlockFrame::new());
			},
			Instruction::Bind => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				stack.last_mut().unwrap().locals.push(var);
				ptr -= 1;
			},
			Instruction::Push(ref var) => {
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(var.clone());
				ptr += 1;
			},
			Instruction::Pop => {
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				ptr -= 1;
			},
			Instruction::Load(ref index) => {
				let var = stack.last_mut().unwrap().locals.get(*index).unwrap().clone();
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(var.clone());
				ptr += 1;
			},
			Instruction::Debug => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				println!("\t => Var: {:?}", var);
				ptr -= 1;
			},
			Instruction::Call(ref index, ref argc) => {
				let mut new_frame = StackFrame::new(instr_ptr);
				for _ in 0..*argc {
					let arg_val = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
					new_frame.locals.push(arg_val);
				}
				// Start a new stack frame
				stack.push(new_frame);				
				// Jump to the index pointed to by the call instruction
				instr_ptr = *index;
				continue;
			},
			Instruction::Set(ref index) => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				*stack.last_mut().unwrap().locals.get_mut(*index).unwrap() = var;
				ptr -= 1;
			},
			Instruction::Output => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let len = stack.last().unwrap().block_frames.len();
				stack.last_mut().unwrap().block_frames.get_mut(len - 2).unwrap().operands.push(var);
				ptr -= stack.last().unwrap().block_frames.last().unwrap().operands.len();
				stack.last_mut().unwrap().block_frames.pop().unwrap();
				// TODO drop all items in stack
			},
			Instruction::Return => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let len = stack.len();
				stack.get_mut(len - 2).unwrap().block_frames.last_mut().unwrap().operands.push(var);
				for block_frame in &stack.last().unwrap().block_frames {
					ptr -= block_frame.operands.len()
				}
				let old_frame = stack.pop().unwrap();
				instr_ptr = old_frame.return_to_ptr;
			}
			Instruction::Add => {
				let right = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let left = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let output = match (left.var_info, right.var_info) {
					(VarInfo::Primitive(Primitive::U64(left)), VarInfo::Primitive(Primitive::U64(right))) => {
						left + right
					},
					_ => panic!("Tried to add types that don't support it") // This is ok now because the conversion to hir changes operators to use the add method when they're not primitives
				};
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(Var::new_u64(output));
				ptr -= 1;
			},
			Instruction::Sub => {
				let right = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let left = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let output = match (left.var_info, right.var_info) {
					(VarInfo::Primitive(Primitive::U64(left)), VarInfo::Primitive(Primitive::U64(right))) => {
						left - right
					},
					_ => panic!("Tried to add types that don't support it") // This is ok now because the conversion to hir changes operators to use the add method when they're not primitives
				};
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(Var::new_u64(output));
				ptr -= 1;
			},
			Instruction::Mul => {
				let right = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let left = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let output = match (left.var_info, right.var_info) {
					(VarInfo::Primitive(Primitive::U64(left)), VarInfo::Primitive(Primitive::U64(right))) => {
						left * right
					},
					_ => panic!("Tried to add types that don't support it") // This is ok now because the conversion to hir changes operators to use the add method when they're not primitives
				};
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(Var::new_u64(output));
				ptr -= 1;
			},
			Instruction::Div => {
				let right = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let left = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let output = match (left.var_info, right.var_info) {
					(VarInfo::Primitive(Primitive::U64(left)), VarInfo::Primitive(Primitive::U64(right))) => {
						left / right
					},
					_ => panic!("Tried to add types that don't support it") // This is ok now because the conversion to hir changes operators to use the add method when they're not primitives
				};
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(Var::new_u64(output));
				ptr -= 1;
			},
			Instruction::Equal => {
				let right = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let left = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let output = Var::new_bool(left == right);
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(output);
				ptr -= 1;
			},
			Instruction::Jump(target_instr_ptr) => {
				instr_ptr = target_instr_ptr;
				continue;
			},
			Instruction::Check(target_instr_ptr) => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				ptr -= 1;
				// If the value is false jump past the if block
				if var.is_false() {
					instr_ptr = target_instr_ptr;
					continue;
				}
			},
			Instruction::Retrieve(ref idx) => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				ptr -= 1;
				match var.var_info {
					VarInfo::Reference(ref reference) => {
						if let Some(field) = reference.fields.get(*idx) {
							stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(field.clone());
						}
					},
					_ => panic!("Field doesn't exist")
				}
			},
			Instruction::Ref(ref amt) => {
				let mut buf = Vec::new();
				for _ in 0..*amt {
					buf.insert(0, stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap());
				}
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(Var::new_ref(Reference::new(buf)));
			},
			Instruction::Terminate => {
				break;
			},
		}

		instr_ptr += 1;

		if debug {
			print_stack(&stack);
		}
	}

	Ok(stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap())
}

fn print_stack(stack: &[StackFrame]) {
	for (i, frame) in stack.iter().enumerate() {
		println!("\tFrame {}", i);
		println!("\t- Locals:");
		for (i, local) in frame.locals.iter().enumerate() {
			println!("\t\t{}: {:?}", i, local);
		}
		println!("\t- Blocks:");
		for (i, block_frame) in frame.block_frames.iter().enumerate() {
			println!("\t\t{}: Operands:", i);
			for operand in &block_frame.operands {
				println!("\t\t\t{:?}", operand);
			}
		}
		println!("");
	}
}